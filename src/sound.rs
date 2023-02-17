use wasm_bindgen::JsCast;
use crate::utils::{JsResult, ResultUtils, JsResultUtils, Tee, Pipe};
use crate::input;
use crate::{MainCmd, sound_comps};
use std::rc::Rc;
// TODO: implement an `StrVec` as an array, optimized for holding strings

pub enum SoundFunctorType {
    Wave {gen: web_sys::OscillatorNode},
    Envelope {
        gen: web_sys::GainNode,
        attack: f64,
        decay: f64,
        sustain: f64,
        release: f64},
    BuiltinOutput {gen: Rc<web_sys::AnalyserNode>}
}

pub struct SoundFunctor {
    functor_type: SoundFunctorType,
    id: usize,
    x: i32,
    y: i32,
    forwards: Vec<usize>
}

impl PartialEq for SoundFunctor {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for SoundFunctor {}

impl std::ops::Deref for SoundFunctor {
    type Target = web_sys::AudioNode;
    fn deref(&self) -> &Self::Target {
        match self.functor_type {
            SoundFunctorType::Wave {ref gen} => gen,
            SoundFunctorType::Envelope {ref gen, ..} => gen,
            SoundFunctorType::BuiltinOutput {ref gen} => gen}
    }
}

impl SoundFunctor {
    pub const MAX_FREQ: f64 = 5000.0;
    pub const DEF_FREQ: f32 = 440.0;
    pub const MAX_INTERVAL: f64 = 2.0;
	pub const MAX_VOLUME: f32 = 0.2;
	pub const MIN_VOLUME: f32 = f32::MIN_POSITIVE;

    pub fn new_wave(player: &web_sys::AudioContext, id: usize, x: i32, y: i32) -> JsResult<Self> {
        let gen = web_sys::OscillatorNode::new_with_options(player, 
            web_sys::OscillatorOptions::new().frequency(Self::DEF_FREQ))?;
        gen.start()?;
        Ok(Self{functor_type: SoundFunctorType::Wave{gen}, 
            id, x, y, forwards: vec![]})
    }

    pub fn new_envelope(player: &web_sys::AudioContext, id: usize, x: i32, y: i32) -> JsResult<Self> {
        let gen = web_sys::GainNode::new_with_options(player, 
            web_sys::GainOptions::new().gain(Self::MIN_VOLUME))?;
        Ok(Self{functor_type: SoundFunctorType::Envelope{gen, attack: 0.0, decay: 0.0, sustain: 0.0, release: 0.0},
            id, x, y, forwards: vec![]})
    }

    pub fn new_builtin_output(gen: Rc<web_sys::AnalyserNode>, id: usize, x: i32, y: i32) -> JsResult<Self> {
        Ok(Self{functor_type: SoundFunctorType::BuiltinOutput{gen},
            id, x, y, forwards: vec![]})
    }

    pub fn start(&mut self, cur_time: f64) -> JsResult<()> {
        Ok(match &self.functor_type {
            SoundFunctorType::Envelope{gen, attack, decay, sustain, ..}
                => _ = gen.gain().cancel_scheduled_values(0.0)
                .add_msg_to_err("resetting the volume control")?
                .linear_ramp_to_value_at_time(Self::MAX_VOLUME, cur_time + attack)
                .add_msg_to_err("setting the attack period")?
                .linear_ramp_to_value_at_time(Self::MAX_VOLUME * *sustain as f32,
                    cur_time + attack + decay)
                .add_msg_to_err("setting the decay period")?,
            _ => ()})
    }

    pub fn end(&mut self, cur_time: f64) -> JsResult<()> {
        Ok(match &self.functor_type {
            SoundFunctorType::Envelope{gen, release, ..}
                => _ = gen.gain().cancel_scheduled_values(0.0)
                .add_msg_to_err("resetting the envelope for the fade-out")?
                .linear_ramp_to_value_at_time(Self::MIN_VOLUME, cur_time + release)
                .add_msg_to_err("setting the volume fade-out")?,
            _ => ()})
    }

    pub fn set_param(&mut self, param_id: usize, value: f64, cur_time: f64) -> JsResult<()> {
        match &mut self.functor_type {
            SoundFunctorType::Wave {gen} => match param_id {
                0 => Ok(_ = gen.frequency().set_value_at_time(value as f32, cur_time)
                    .expect_throw_val("setting the frequency the `Frequency` parameter of a `Wave` sound functor")
                    .value().js_log("new frequency: ")),
                1 => match value as usize {
                    0 => Ok(gen.set_type(web_sys::OscillatorType::Sine)),
                    1 => Ok(gen.set_type(web_sys::OscillatorType::Square)),
                    2 => Ok(gen.set_type(web_sys::OscillatorType::Sawtooth)),
                    3 => Ok(gen.set_type(web_sys::OscillatorType::Triangle)),
                    _ => Err(js_sys::Error::new("invalid wave type").into())}
                _ => Err(js_sys::Error::new("invalid parameter ID of `SoundFunctor::Wave`").into())}
            SoundFunctorType::Envelope {attack, decay, sustain, release, ..} => match param_id {
                0 => Ok(*attack = value),
                1 => Ok(*decay = value),
                2 => Ok(*sustain = value),
                3 => Ok(*release = value),
                _ => Err(js_sys::Error::new("invalid parameter ID of `SoundFunctor::Envelope`").into())}
            SoundFunctorType::BuiltinOutput{..}
                => Err(js_sys::Error::new("cannot set a parameter on `SoundFunctor::BuiltinOutput`").into())}
    }

    #[inline] pub fn forwards<'a>(&'a self) -> Option<&'a [usize]> {
        match self.functor_type {
            SoundFunctorType::BuiltinOutput{..} => None,
            _ => Some(&*self.forwards)}
    }

    #[inline] pub fn backwardable(&self) -> bool {
        match self.functor_type {
            SoundFunctorType::Wave {..} => false,
            _ => true}
    }

    pub fn connect(&mut self, other: &SoundFunctor) -> JsResult<Option<()>> {
        if !other.backwardable() || self.forwards().filter(|x| !x.contains(&other.id)).is_none() {return Ok(None)}
        self.forwards.push(other.id);
        self.connect_with_audio_node(other)?;
        Ok(Some(()))
    }

    pub fn disconnect(&mut self, other: &SoundFunctor) -> JsResult<Option<()>> {
        if !self.forwards.contains(&other.id) {return Ok(None)}
        self.disconnect_with_audio_node(other)?;
        self.forwards.retain(|&x| x != other.id);
        Ok(Some(()))
    }

    pub fn name(&self) -> &'static str {
        match &self.functor_type {
            SoundFunctorType::Wave{..} => "Wave generator",
            SoundFunctorType::Envelope{..} => "Envelope",
            SoundFunctorType::BuiltinOutput{..} => "Output"}
    }

    #[inline] pub fn location(&self) -> [i32; 2] {[self.x, self.y]}

    pub fn contains(&self, x: i32, y: i32) -> bool {
        (self.x - SoundBlock::SIZE / 2 ..= self.x + SoundBlock::SIZE / 2).contains(&x)
        && (self.y - SoundBlock::SIZE / 2 ..= self.y + SoundBlock::SIZE / 2).contains(&y)
    }

    pub fn params(&self) -> yew::Html {
        match &self.functor_type {
            SoundFunctorType::Wave{gen} => yew::html!{<>
                <input::Slider
                    id={0}
                    coef={SoundFunctor::MAX_FREQ} precision={0}
                    postfix={"Hz"}
                    name={Rc::from("Frequency")}
                    component_id={self.id}
                    initial={gen.frequency().value() as f64}/>
                <input::Switch
                    id={1}
                    options={vec!["Sine".into(), "Square".into(), "Saw".into(), "Triangle".into()]}
                    name={Rc::from("Wave type")}
                    component_id={self.id}
                    initial={match gen.type_() {
                        web_sys::OscillatorType::Sine => 0,
                        web_sys::OscillatorType::Square => 1,
                        web_sys::OscillatorType::Sawtooth => 2,
                        web_sys::OscillatorType::Triangle => 3,
                        _ => wasm_bindgen::throw_str("found an invalid wave type while getting it to generate component's parameter list")}}/>
            </>},
            SoundFunctorType::Envelope{attack, decay, sustain, release, ..} => yew::html! {<>
                <input::Slider
                    id={0}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={Rc::from("Attack time")}
                    component_id={self.id}
                    initial={attack}/>
                <input::Slider
                    id={1}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={Rc::from("Decay time")}
                    component_id={self.id}
                    initial={decay}/>
                <input::Slider
                    id={2}
                    coef={1.0}
                    name={Rc::from("Sustain level")}
                    component_id={self.id}
                    initial={sustain}/>
                <input::Slider
                    id={3}
                    coef={SoundFunctor::MAX_INTERVAL}
                    postfix={"s"}
                    name={Rc::from("Release time")}
                    component_id={self.id}
                    initial={release}/>
            </>},
            SoundFunctorType::BuiltinOutput{..} => Default::default()}
    }

    pub fn as_html(&self) -> yew::Html {
        yew::html!{
            <SoundBlock id={self.id} name={Rc::from(self.name())}/>}
    }

    pub fn graph(&self, width: f64, height: f64) -> JsResult<(web_sys::Path2d, f64, f64)> {
        match &self.functor_type {
            SoundFunctorType::Envelope {attack, decay, sustain, release, ..} => {
                let res = (web_sys::Path2d::new()?, attack + decay);
                let res = (res.0, res.1, res.1 + release);
                res.0.move_to(0.0, height);
                res.0.line_to(attack / res.2 * width, 0.0);
                res.0.line_to(res.1 / res.2 * width, (1.0 - sustain) as f64 * height);
                res.0.line_to(width, height);
                Ok(res).js_log("new graph")}
            _ => Ok((web_sys::Path2d::new()?, f64::NAN, f64::NAN))
        }
    }

    pub fn graphable(&self) -> bool {
        match &self.functor_type {
            SoundFunctorType::Envelope {..} => true,
            _ => false
        }
    }
}

pub struct SoundBlock {
    pending_conn_coords: Option<[i32; 2]>,
    visual_points: Rc<str>
}

#[derive(yew::Properties)]
pub struct SoundBlockProps {
    id: usize,
    name: Rc<str>,
}

impl PartialEq for SoundBlockProps {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub enum SoundBlockCmd {
    Focus(web_sys::PointerEvent),
    Unfocus(web_sys::PointerEvent),
    DragConnection(web_sys::PointerEvent),
    MaybeShowHelp(),
    MaybeHideHelp()
}

// the `const_format` crate doesn't handle namespace fields
//  (i.e. `Self::SIZE`) so I had to use this disgusting hack
macro_rules! soundblock_impl { 
    (SIZE: $v:literal) => {
        impl SoundBlock {
            const SIZE: i32 = $v;
            const SIZE_STR: &str = stringify!($v);
            pub fn get_polygon(comp: &SoundFunctor) -> String {
                format!("{},{} ", comp.x, comp.y - Self::SIZE / 2)
                    + &comp.forwards().map(|_| format!("{},{} ", comp.x + Self::SIZE / 2, comp.y))
                        .unwrap_or_default()
                    + &format!("{},{} ", comp.x, comp.y + Self::SIZE / 2)
                    + &comp.backwardable().then(|| format!("{},{} ", comp.x - Self::SIZE / 2, comp.y))
                        .unwrap_or_default()
            }
        }
    };
}
soundblock_impl!(SIZE: 64);

impl yew::Component for SoundBlock {
    type Message = SoundBlockCmd;
    type Properties = SoundBlockProps;

    fn create(ctx: &yew::Context<Self>) -> Self {
        let comp = sound_comps().get(ctx.props().id)
            .expect_throw_with(|| format!("invalid component id of {} passed to a UI sound block", ctx.props().id));
        Self{pending_conn_coords: None,
            visual_points: Self::get_polygon(comp).into()}
    }

    fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        if let Some(comp) = sound_comps().get(ctx.props().id) {
            let fwds = comp.forwards();
            let [x1, y1] = comp.location().pipe(|[x, y]| [x + Self::SIZE / 2, y])
                .map(|i| Rc::from(i.to_string()));
            yew::html! {<>
                <polygon points={self.visual_points.clone()}
                    width={Self::SIZE_STR} height={Self::SIZE_STR}
                    class="component"
                    onpointerenter={ctx.link().callback(|_| SoundBlockCmd::MaybeShowHelp())}
                    onpointerleave={ctx.link().callback(|_| SoundBlockCmd::MaybeHideHelp())}
                    onpointerdown={fwds.map(|_| ctx.link().callback(SoundBlockCmd::Focus))}
                    onpointerup={ctx.link().callback(SoundBlockCmd::Unfocus)}
                    onpointermove={self.pending_conn_coords.is_some()
                        .then(|| ctx.link().callback(SoundBlockCmd::DragConnection))}/>
                if let Some(fwds) = fwds {
                    if let Some([x2, y2]) = self.pending_conn_coords {
                        <line class="component"
                            x1={Rc::clone(&x1)} y1={Rc::clone(&y1)}
                            x2={x2.to_string()} y2={y2.to_string()}/>
                    }
                    {for fwds.iter()
                        .flat_map(|i| sound_comps().get(*i))
                        .map(|c| c.location())
                        .map(|[x2, y2]| yew::html!{
                            <line class="component"
                                x1={x1.clone()}     y1={y1.clone()}
                                x2={x2.to_string()} y2={y2.to_string()}/>})}
                }
            </>}
        } else {Default::default()}
    }

    fn update(&mut self, ctx: &yew::Context<Self>, msg: Self::Message) -> bool {
        let SoundBlockProps {id, name, ..} = ctx.props();
        match msg {
            SoundBlockCmd::Focus(e) => {
                let target = e.target()
                    .expect_throw("fetching the input element in the `pointerdown` event")
                    .unchecked_into::<web_sys::HtmlElement>();
                target.set_pointer_capture(e.pointer_id())
                    .expect_throw_val("setting the cursor focus on the sound component");
                self.pending_conn_coords = Some([e.x(), e.y()]);
                true}
            SoundBlockCmd::Unfocus(e) => {
                let [x, y] = [e.x(), e.y()];
                match sound_comps().iter().find(|c| c.contains(x, y)) {
                    Some(c) => MainCmd::Select(Some(c.id)),
                    None    => MainCmd::RemoveDesc()
                }.send();
                if sound_comps().get(*id).and_then(|c| c.forwards()).is_none() {return false}
                let target = e.target()
                    .expect_throw("fetching the input element in the `pointerup` event")
                    .unchecked_into::<web_sys::HtmlElement>();
                target.release_pointer_capture(e.pointer_id())
                    .expect_throw_val("releasing the cursor focus from the sound component");
                self.pending_conn_coords = None;
                MainCmd::TryConnect(*id, x, y).send();
                true}
            SoundBlockCmd::DragConnection(e) => {
                self.pending_conn_coords = Some([e.x(), e.y()]);
                true}
            SoundBlockCmd::MaybeShowHelp() => {
                MainCmd::SetDesc(name.clone()).send();
                false}
            SoundBlockCmd::MaybeHideHelp() => {
                MainCmd::RemoveDesc().send();
                false}
        }
    }
}

