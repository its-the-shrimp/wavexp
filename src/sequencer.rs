use std::{cmp::Reverse, fmt::Debug};
use web_sys::AnalyserNode as JsAnalyserNode;
use crate::{
    sound::{SoundGen, Note, SoundPlayer, Secs, Sound, MSecs},
    utils::{JsResult, JsResultUtils, R64, OptionExt, VecExt, Take, ResultToJsResult},
    input::ParamId,
    loc, r32, r64
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct SequencerEvent {
    pub when: MSecs,
    pub element_id: usize
}

/// resposible for managing the sound elements and visualising/triggering them on request
pub struct Sequencer {
    elements: Vec<SoundGen>,
    connections: Vec<Vec<usize>>,
    events: Vec<SequencerEvent>,
    sound_player: SoundPlayer,
    ending: bool,
    starting_note: Note
}

fn traverse<S: Clone + Debug>(conns: &[Vec<usize>], src_id: usize, state: S, f: &mut impl FnMut(S, usize, usize) -> JsResult<Option<S>>)
-> JsResult<()> {
    let dsts = conns.get(src_id).to_js_result(loc!())?;
    for dst_id in dsts.iter() {
        let Some(new_state) = f(state.clone(), src_id, *dst_id).add_loc(loc!())?
            else {return Ok(())};
        traverse(conns, *dst_id, new_state, f).add_loc(loc!())?;
    }
    Ok(())
}

impl Sequencer {
    pub fn new() -> JsResult<Self> {
        let mut sound_player = SoundPlayer::new(r64![120.0], r32![0.2]).add_loc(loc!())?;
        Ok(Self{elements: vec![SoundGen::new_input(), SoundGen::new_output(&mut sound_player)],
            connections: vec![vec![], vec![]], events: vec![], sound_player,
            starting_note: Note::A2, ending: false})
    }

    #[inline] pub fn visualiser(&self) -> &JsAnalyserNode {
        self.sound_player.visualiser()
    }

    #[inline] pub fn elements(&self) -> &[SoundGen] {&self.elements}

    #[inline] pub fn elements_mut(&mut self) -> &mut [SoundGen] {&mut self.elements}

    #[inline] pub fn connections(&self) -> &[Vec<usize>] {
        &self.connections
    }

    fn connect(&mut self, src_id: usize, dst_id: usize) -> JsResult<()> {
        let src = self.elements.get(src_id).to_js_result(loc!())?;
        let dst = self.elements.get(dst_id).to_js_result(loc!())?;
        if src.connectible(dst) {
            unsafe{self.connections.get_unchecked_mut(src_id)}
                .push_unique(dst_id, usize::eq);
        }
        Ok(())
    }

    /// the returned `bool` indicates whether the selected element's editor window should be
    /// rerendered
    pub fn set_param(&mut self, id: ParamId, value: R64) -> JsResult<bool> {
        match id {
            ParamId::Play(note) => if value.is_sign_positive() {
                self.starting_note = note;
                for element in self.elements.iter_mut() {
                    element.reset(&mut self.sound_player);
                }
                self.events.push(SequencerEvent{element_id: 0, when: R64::NEG_INFINITY});
            } else {
                self.ending = true;
            }

            ParamId::Connect(id1, id2) => self.connect(id1, id2)?,

            ParamId::Disconnect(id) => self.connections
                .get_mut(id).to_js_result(loc!())?.clear(),

            ParamId::Add(init, _) => {
                self.elements.push(init());
                self.connections.push(vec![]);
            }

            ParamId::Remove(id) => {
                self.elements.try_swap_remove(id).to_js_result(loc!())?;
                self.connections.try_swap_remove(id).to_js_result(loc!())?;
                let len = self.connections.len();
                for conns in self.connections.iter_mut() {
                    conns.retain_mut(|x| {
                        if *x == id {return false}
                        if *x == len {*x = id}
                        true});
                }
            }

            id => {
                if let Some(element_id) = id.element_id() {
                    return self.elements.get_mut(element_id).to_js_result(loc!())?
                        .set_param(id, value)
                }
            }
        };
        Ok(false)
    }

    /*fn draw(&mut self) -> JsResult<()> {
        let (w, h, ctx) = (self.canvas.width() as f64,
            self.canvas.height() as f64,
            self.canvas.get_2d_context(loc!())?);
        ctx.fill_rect(0.0, 0.0, w, h);
        ctx.begin_path();
        self.elements.iter()
            .for_each(|c| c.draw(&ctx, self.offset));
        for (src_id, dsts) in self.connections.iter().enumerate() {
            let Some(mut src) = self.elements
                .get(src_id).to_js_result(loc!())?
                .output_point() else {continue};
            src -= self.offset;
            for dst_id in dsts.iter() {
                let Some(mut dst) = self.elements
                    .get(*dst_id).to_js_result(loc!())?
                    .input_point() else {continue};
                dst -= self.offset;
                ctx.move_to(src.x as f64, src.y as f64);
                ctx.line_to(dst.x as f64, dst.y as f64);
            }
        }
        Ok(ctx.stroke())
    }*/

    pub fn poll(&mut self, time: Secs) -> JsResult<()> {
        if self.ending.take() {
                self.events.clear();
                self.sound_player.end_sounds();
        } else {
            let len = self.events.len();
            let start = match self.events.iter().rev().position(|event| time < event.when) {
                Some(x) => (x > 0).then_some(len - x),
                None => Some(0)};
            if let Some(start) = start {
                let pending: Vec<_> = self.events.drain(start..).collect();
                for event in pending {
                    let mut apply_sound_comp = |sound: Sound, _: usize, element_id: usize| -> JsResult<Option<Sound>> {
                        let (sound, when) = self.elements
                            .get_mut(element_id).to_js_result(loc!())?
                            .transform(&mut self.sound_player, sound, time).add_loc(loc!())?;
                        if let Some(when) = when {
                            self.events.push_sorted_by_key(SequencerEvent{when, element_id}, |x| Reverse(*x));
                        }
                        Ok(sound)
                    };
                    let Some(sound) = apply_sound_comp(Sound::InputNote(self.starting_note, None), 0, event.element_id).add_loc(loc!())?
                        else {continue};
                    traverse(&self.connections, event.element_id, sound, &mut apply_sound_comp).add_loc(loc!())?;
                }
            }
        }
        self.sound_player.poll(time / 1000.0).add_loc(loc!())?;
        Ok(())
    }
}
