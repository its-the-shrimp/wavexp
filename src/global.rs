use std::{
    rc::Rc,
    borrow::Cow};
use js_sys::Function;
use wasm_bindgen::{
    closure::Closure,
    JsCast};
use web_sys::{
    PointerEvent,
    MouseEvent,
    AudioContext,
    UiEvent};
use yew::{
    Component,
    Context,
    Html,
    html};
use crate::{
    sound::{MSecs, Secs, Beats, SoundType, TabInfo, Sound, Sequencer, PatternBlock},
    visual::{HintHandler, SoundVisualiser, Graphable},
    utils::{R64, R32, JsResultUtils, window, OptionExt, SliceExt, JsResult, BoolExt},
    input::{Button, Slider, Switch},
    loc, r64, js_try};

/// the all-encompassing event type for the app
#[derive(Debug, PartialEq, Clone)]
pub enum AppEvent {
    /// emitted every frame, i.e. roughly every 17 ms
    /// the field is the current time, but in milliseconds, unlike `AppContext::now`
    Frame(MSecs),
    /// emitted by the selected sound bloock when its visual representation is expected to have
    /// changed
    RedrawEditorPlane,
    /// emitted when the user switches tabs in the side editor
    SetTab(usize),
    /// epilog variant of `SetTab`
    AfterSetTab(usize),
    /// emitted when the viewport of the app changes its dimensions
    Resize,
    /// emitted when the user clicks the `Play` button
    TogglePlay,
    /// epllog version of `TogglePlay`
    AfterTogglePlay,
    /// emitted when the user selects a sound block to edit in the side editor
    Select(Option<usize>),
    /// epliog version of `Select`
    AfterSelect(Option<usize>),
    /// emitted when the user adds a sound block
    Add(SoundType, i32, Beats),
    /// emitted when the user deletes the selected sound block
    Remove,
    /// epilog version of `Remove`
    AfterRemove,
    /// emitted when a `Noise` sound block's duration has been changed
    Duration(R64),
    /// emitted when a `Noise` sound block's volume ha been changed
    Volume(R32),
    /// emitted when the global BPM has been changed
    Bpm(R64),
    /// emitted when the global volume has been changed
    MasterGain(R32),
    /// emitted when the global editor snap step has been changed
    SnapStep(R64),
    /// emitted when the user focuses the main editor plane i.e. by holding left click
    FocusPlane(PointerEvent),
    /// emitted when the user moves the cursor across the main editor plane
    HoverPlane(MouseEvent),
    /// emitted when the user drags the cursor out of the main editor plane
    LeavePlane,
    /// emitted when the user starts dragging a new sound block 
    SetBlockAdd(Option<SoundType>),
    /// emitted when the user focuses the side editor plane i.e. by holding left click
    FocusTab(PointerEvent),
    /// emitted when the user moves the cursor across the side editor plane
    HoverTab(MouseEvent),
    /// emitted when the user drags the cursor out of the side editor plane
    LeaveTab,
    /// emitted to set the hint for the user
    /// 1st is the main, shorter, hint, 2nd is the auxillary, longer, hint
    SetHint(Cow<'static, str>, Cow<'static, str>),
    /// similar to `SetHint` but gets the hint from an event's target
    FetchHint(UiEvent)
}

impl AppEvent {
    /// returns an event that should be returned after re-rendering the page layout
    /// if page layout re-render is not needed, `None` is returned
    #[inline] pub fn epilog(&self) -> Option<Self> {
        match self {
            Self::SetTab(id)   => Some(Self::AfterSetTab(*id)),
            | Self::Select(id) => Some(Self::AfterSelect(*id)),
            | Self::TogglePlay => Some(Self::AfterTogglePlay),
            | Self::Remove     => Some(Self::AfterRemove),

            Self::Frame(..)
            | Self::AfterSetTab(..)
            | Self::AfterSelect(..)
            | Self::AfterTogglePlay
            | Self::AfterRemove
            | Self::SetHint(..)
            | Self::FetchHint(..)
            | Self::RedrawEditorPlane
            | Self::Duration(..)
            | Self::Resize 
            | Self::Add(..) 
            | Self::Volume(..) 
            | Self::Bpm(..) 
            | Self::MasterGain(..) 
            | Self::SnapStep(..) 
            | Self::FocusPlane(..) 
            | Self::LeavePlane 
            | Self::HoverPlane(..) 
            | Self::SetBlockAdd(..) 
            | Self::FocusTab(..) 
            | Self::HoverTab(..) 
            | Self::LeaveTab => None
        }
    }
}

/// carries all the app-wide settings that are passed to all the event receivers
pub struct AppContext {
    pub bps: Beats,
    pub play_since: Secs,
    pub now: Secs,
    pub offset: Beats,
    pub snap_step: R64,
    pub selected_tab: usize
}

impl AppContext {
    pub fn new(bps: Beats, snap_step: R64) -> Self {
        Self{bps, offset: r64![0.0], snap_step,
            play_since: Secs::NEG_INFINITY, now: Secs::NEG_INFINITY,
            selected_tab: 0}
    }

    pub fn handle_event(&mut self, event: &AppEvent) {
        match event {
            AppEvent::Bpm(bpm) =>
                self.bps = *bpm / 60u8,
            AppEvent::TogglePlay =>
                self.play_since = self.play_since.is_finite().choose(Secs::NEG_INFINITY, self.now),
            AppEvent::Frame(now) =>
                self.now = *now / 1000u16,
            AppEvent::SnapStep(value) =>
                self.snap_step = *value,
            AppEvent::Select(_) =>
                self.selected_tab = 0,
            AppEvent::SetTab(id) =>
                self.selected_tab = *id,
            _ => ()
        }
    }
}

pub struct App {
    sound_visualiser: SoundVisualiser,
    sequencer: Sequencer,
    ctx: AppContext,
    hint_handler: HintHandler,
    frame_emitter: Function,
    epilog: Option<AppEvent>
}

impl Component for App {
    type Message = AppEvent;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let cb = ctx.link().callback(AppEvent::Frame);
        let ctx = AudioContext::new().unwrap_throw(loc!());
        let sound_visualiser = SoundVisualiser::new(&ctx).unwrap_throw(loc!());

        let res = Self{epilog: None,
            hint_handler: HintHandler::default(),
            ctx: AppContext::new(r64![2.0], r64![1.0]),
            sequencer: Sequencer::new(ctx, Rc::clone(sound_visualiser.input())).unwrap_throw(loc!()),
            sound_visualiser,
            frame_emitter: Closure::<dyn Fn(f64)>::new(move |x| cb.emit(R64::new_or(r64![0.0], x)))
                .into_js_value().unchecked_into()};
        window().request_animation_frame(&res.frame_emitter).unwrap_throw(loc!());
        res
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        _ = js_try!{type = !:
            match msg {
                AppEvent::Frame(_) => _ = window()
                    .request_animation_frame(&self.frame_emitter).add_loc(loc!())?,

                AppEvent::Remove => self.sequencer.pattern_mut()
                    .del_fixed().to_js_result(loc!())?,

                _ => ()
            }
            self.epilog = msg.epilog();
            self.forward_event(ctx, msg).add_loc(loc!())?;
            return self.epilog.is_some()
        }.report_err(loc!());
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let block = self.sequencer.pattern().fixed();

        let setter = ctx.link().callback(|x| x);
        let render_tab_info = |info: &TabInfo, tab_id: usize, desc: String| -> Html {
            html!{
                <div id={(self.ctx.selected_tab == tab_id).then_some("selected-tab")}
                onpointerup={ctx.link().callback(move |_| AppEvent::SetTab(tab_id))}
                data-main-hint={info.name} data-aux-hint={desc}>
                    <p>{info.name}</p>
                </div>
            }
        };

        html! {<>
            <div id="main-panel">
                <div id="ctrl-panel" class="dark-bg"
                data-main-hint="Settings" data-aux-hint={block.map_or_else(|| "General".to_owned(), PatternBlock::desc)}>
                    <div id="hint" class="light-bg"
                    data-main-hint="Hint bar" data-aux-hint="for useful messages about the app's controls">
                        <span id="main-hint" ref={self.hint_handler.main_bar().clone()}/>
                        <br/>
                        <span id="aux-hint" ref={self.hint_handler.aux_bar().clone()}/>
                    </div>
                    if let Some((tab_aux_hint, block)) = block.map(|x| (x.desc() + ": Settings tab", x)) {
                        <div id="tab-list">
                            {for block.sound.tabs().iter().enumerate()
                                .map(|(tab_id, tab)| render_tab_info(tab, tab_id, tab_aux_hint.clone()))}
                        </div>
                        {block.sound.params(self.ctx.selected_tab, setter.clone())}
                        <div id="general-ctrl" class="dark-bg">
                            <Button name="Back to project-wide settings"
                            setter={setter.reform(|_| AppEvent::Select(None))}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="20,60 50,20 80,60 70,60 70,80 30,80 30,60"/>
                                </svg>
                            </Button>
                            <Button name="Remove component"
                            setter={setter.reform(|_| AppEvent::Remove)}>
                                <svg viewBox="0 0 100 100">
                                    <polygon points="27,35 35,27 50,42 65,27 73,35 58,50 73,65 65,73 50,58 35,73 27,65 42,50"/>
                                </svg>
                            </Button>
                        </div>
                    } else {
                        // TODO: fix tilted edges of the borders of the tab menu
                        <div id="tab-list">
                            {render_tab_info(&TabInfo{name: "General"}, 0, "Settings tab".to_owned())}
                            {render_tab_info(&TabInfo{name: "Add block"}, 1, "Settings tab".to_owned())}
                        </div>
                        if self.ctx.selected_tab == 0 {
                            <div id="inputs">
                                <Slider key="tmp" name="Tempo"
                                    setter={setter.reform(AppEvent::Bpm)}
                                    min={r64![30.0]} max={r64![240.0]}
                                    postfix="BPM"
                                    initial={self.ctx.bps * r64![60.0]}/>
                                <Slider key="gain" name="Master gain level"
                                setter={setter.reform(|x| AppEvent::MasterGain(R32::from(x)))}
                                    initial={R64::from(self.sequencer.gain())}/>
                            </div>
                        } else if self.ctx.selected_tab == 1 {
                            <div id="block-add-menu">
                                {for Sound::TYPES.iter().map(|x| html!{
                                    <div draggable="true"
                                    data-main-hint={x.name()} data-aux-hint="Hold and drag to add block to plane"
                                    ondragstart={ctx.link().callback(|_| AppEvent::SetBlockAdd(Some(*x)))}
                                    ondragend={ctx.link().callback(|_| AppEvent::SetBlockAdd(None))}>
                                        <p>{x.name()}</p>
                                    </div>
                                })}
                            </div>
                        }
                    }
                </div>
                <canvas ref={self.sequencer.canvas().clone()} id="plane"
                onpointerdown={ctx.link().callback(    AppEvent::FocusPlane)}
                onpointerup={ctx.link().callback(|e|   AppEvent::HoverPlane(MouseEvent::from(e)))}
                onpointermove={ctx.link().callback(|e| AppEvent::HoverPlane(MouseEvent::from(e)))}
                onpointerout={ctx.link().callback(|_|  AppEvent::LeavePlane)}
                ondragover={ctx.link().callback(|e|    AppEvent::HoverPlane(MouseEvent::from(e)))}
                ondragleave={ctx.link().callback(|_|   AppEvent::LeavePlane)}
                ondrop={ctx.link().callback(|e|        AppEvent::HoverPlane(MouseEvent::from(e)))}/>
            </div>
            <div id="io-panel" data-main-hint="Editor plane settings">
                <div id="plane-settings" data-main-hint="Editor plane settings">
                    <Switch key="snap" name="Interval for blocks to snap to"
                    setter={setter.reform(|x: usize|
                        AppEvent::SnapStep(*[r64![0.0], r64![1.0], r64![0.5], r64![0.25], r64![0.125]].get_wrapping(x)))}
                    options={vec!["None", "1", "1/2", "1/4", "1/8"]}
                    initial={match *self.ctx.snap_step {
                        x if x == 1.0   => 1,
                        x if x == 0.5   => 2,
                        x if x == 0.25  => 3,
                        x if x == 0.125 => 4,
                        _ => 0
                    }}/>
                </div>
                <Button name="Play"
                setter={setter.reform(|_| AppEvent::TogglePlay)}>
                    <svg viewBox="3 0 100 103" height="100%">
                        if self.ctx.play_since.is_finite() {
                            <polygon points="25,25 35,25 35,75 25,75"/>
                            <polygon points="65,25 75,25 75,75 65,75"/>
                        } else {
                            <polygon points="25,25 75,50 25,75"/>
                        }
                    </svg>
                </Button>
                <canvas id="sound-visualiser" ref={self.sound_visualiser.canvas().clone()} class="blue-border"
                data-main-hint="Sound visualiser"/>
            </div>
            <div id="error-sign" hidden={true}
            data-main-hint="Error has occured" data-aux-hint="Check the console for more info">
                <svg viewBox="0 0 100 100">
                    <polygon points="10,90 50,10 90,90"/>
                    <polygon points="48,40 52,40 52,60 48,60"/>
                    <polygon points="48,70 52,70 52,74 48,74"/>
                </svg>
            </div>
        </>}
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        if let Some(event) = self.epilog.take() {
            _ = self.forward_event(ctx, event).report_err(loc!());
        }

        if !first_render {return}
        let window = window();

        let cb = ctx.link().callback(|_| AppEvent::Resize);
        let cb = Closure::<dyn Fn()>::new(move || cb.emit(()))
            .into_js_value().unchecked_into();
        window.set_onresize(Some(&cb));

        let cb = ctx.link().callback(AppEvent::FetchHint);
        let cb = Closure::<dyn Fn(UiEvent)>::new(move |e| cb.emit(e))
            .into_js_value().unchecked_into();
        window.set_onpointerover(Some(&cb));

        ctx.link().send_message(AppEvent::Resize);
    }
}

impl App {
    fn forward_event(&mut self, ctx: &Context<Self>, event: AppEvent) -> JsResult<()> {
        self.ctx.handle_event(&event);
        self.hint_handler.handle_event(&event, &self.ctx).add_loc(loc!())?;
        self.sound_visualiser.handle_event(&event, &self.ctx).add_loc(loc!())?;
        if let Some(next) = self.sequencer.handle_event(&event, &self.ctx).add_loc(loc!())? {
            ctx.link().send_message(next);
        }
        if let Some(mut block) = self.sequencer.pattern_mut().fixed_mut() {
            self.ctx.offset = block.offset;
            if let Some(next) = block.inner().handle_event(&event, &self.ctx).add_loc(loc!())? {
                ctx.link().send_message(next);
            }
            self.ctx.offset = r64![0.0];
        }
        Ok(())
    }
}