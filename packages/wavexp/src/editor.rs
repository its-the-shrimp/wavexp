use crate::{
    app::AppContext,
    ctx::{AppEvent, ContextMut, ContextRef, EditorAction},
    img,
    input::{Button, GraphEditorCanvas, Switch},
    sequencer::{Sequencer, SoundBlock},
    visual::{HintHandler, SoundVisualiser, SpecialAction},
};
use macro_rules_attribute::apply;
use std::{cmp::Ordering, iter::once, mem::take, slice::from_ref};
use wasm_bindgen::JsCast;
use wavexp_utils::{
    error::Result,
    ext::{default, BoolExt, OptionExt, SliceExt},
    fallible, js_function, r64,
    real::R64,
    window, ToAttrValue,
};
use yew::{html, AttrValue, Callback, Html};

pub struct EditorContext {
    actions: Vec<EditorAction>,
    undid_actions: usize,
    selected_tab: usize,
    snap_step: R64,
    special_action: SpecialAction,
    selected_block: Option<usize>,
}

impl EditorContext {
    pub fn new() -> Self {
        Self {
            actions: vec![EditorAction::Start],
            undid_actions: 0,
            selected_tab: 0,
            snap_step: r64!(1),
            special_action: default(),
            selected_block: None,
        }
    }

    pub fn register_action(&mut self, app: &mut AppContext, action: EditorAction) -> Result {
        app.force_rerender();
        self.actions
            .drain(self.actions.len() - take(&mut self.undid_actions)..);
        if let Some(last) = self.actions.pop() {
            match last.merge(action) {
                Ok(None) => (),
                Ok(Some((first, None))) => self.actions.push(first),
                Ok(Some((first, Some(second)))) => self.actions.extend([first, second]),
                Err((first, second, err)) => {
                    self.actions.extend([first, second]);
                    return Err(err);
                }
            };
        }
        Ok(())
    }
}

impl ContextMut<'_, '_> {
    pub fn selected_tab(&self) -> usize {
        self.editor.selected_tab
    }

    pub fn snap_step(&self) -> R64 {
        self.editor.snap_step
    }

    pub fn special_action(&self) -> SpecialAction {
        self.editor.special_action
    }

    pub fn selected_block(&self) -> Option<usize> {
        self.editor.selected_block
    }

    pub fn actions(&self) -> &[EditorAction] {
        &self.editor.actions
    }
}

pub struct Editor {
    sound_visualiser: SoundVisualiser,
    pub sequencer: Sequencer,
    pub ctx: EditorContext,
    hint_handler: HintHandler,
}

impl Editor {
    pub fn new() -> Result<Self> {
        Ok(Self {
            hint_handler: default(),
            sequencer: Sequencer::new()?,
            sound_visualiser: SoundVisualiser::new(),
            ctx: EditorContext::new(),
        })
    }

    #[apply(fallible!)]
    pub fn handle_event(&mut self, event: &mut AppEvent, app: &mut AppContext) {
        let mut ctx = ContextMut {
            editor: &mut self.ctx,
            app,
        };
        match *event {
            AppEvent::SnapStep(to) => {
                ctx.register_action(EditorAction::SetSnapStep {
                    from: ctx.editor.snap_step,
                    to,
                })?;
                ctx.editor.snap_step = to;
            }

            AppEvent::SetTab(to) => {
                ctx.register_action(EditorAction::SwitchTab {
                    from: ctx.editor.selected_tab,
                    to,
                })?;
                ctx.editor.selected_tab = to;
            }

            AppEvent::SetSpecialAction(action) => {
                ctx.editor.special_action = action;
                ctx.force_rerender();
            }

            AppEvent::KeyPress(_, ref e) if !e.repeat() => match e.code().as_str() {
                "KeyZ" if e.meta_key() => {
                    if e.shift_key() {
                        if ctx.editor.undid_actions > 0 {
                            ctx.force_rerender();
                            let a = unsafe {
                                ctx.editor.actions.get_unchecked(
                                    ctx.editor.actions.len() - ctx.editor.undid_actions,
                                )
                            };
                            ctx.emit_event(AppEvent::Redo(from_ref(a).to_box()));
                            ctx.editor.undid_actions -= 1;
                        }
                    } else if ctx.editor.undid_actions < ctx.editor.actions.len() - 1 {
                        ctx.force_rerender();
                        ctx.editor.undid_actions += 1;
                        let a = unsafe {
                            ctx.editor
                                .actions
                                .get_unchecked(ctx.editor.actions.len() - ctx.editor.undid_actions)
                        };
                        ctx.emit_event(AppEvent::Undo(from_ref(a).to_box()));
                    }
                }

                "KeyA" => ctx.emit_event(AppEvent::SetSpecialAction(SpecialAction::Add)),

                "KeyS" => ctx.emit_event(AppEvent::SetSpecialAction(SpecialAction::Select)),

                "KeyR" => ctx.emit_event(AppEvent::SetSpecialAction(SpecialAction::Remove)),

                _ => (),
            },

            AppEvent::Unwind(n) => {
                ctx.force_rerender();
                let unwound = ctx
                    .editor
                    .actions
                    .get(ctx.editor.actions.len() - n - ctx.editor.undid_actions..)?
                    .iter()
                    .rev()
                    .cloned()
                    .collect();
                ctx.editor.undid_actions += n;
                ctx.emit_event(AppEvent::Undo(unwound))
            }

            AppEvent::Rewind(n) => {
                ctx.force_rerender();
                let rewound = ctx
                    .editor
                    .actions
                    .get(ctx.editor.actions.len() - ctx.editor.undid_actions..)?
                    .get(..n)?
                    .to_box();
                ctx.editor.undid_actions -= n;
                ctx.emit_event(AppEvent::Redo(rewound))
            }

            AppEvent::Select(to) => {
                if to != ctx.editor.selected_block {
                    let prev_selected_tab = take(&mut ctx.editor.selected_tab);
                    ctx.register_action(EditorAction::Select {
                        from: ctx.editor.selected_block,
                        to,
                        prev_selected_tab,
                    })?;
                    ctx.editor.selected_block = to
                }
            }

            AppEvent::Remove => {
                let mut pattern = self.sequencer.pattern().get_mut()?;
                let id = ctx.selected_block()?;
                let block_id = *pattern.selection().get(id)?;
                let action = pattern.remove_points(once(block_id))?;
                ctx.register_action(action)?;
                let from = take(&mut ctx.editor.selected_tab);
                ctx.register_action(EditorAction::SwitchTab { from, to: 0 })?;
            }

            AppEvent::Enter(id, _) => {
                let window = window();
                let cb = ctx.event_emitter();

                let onkeydown = cb.reform(move |e| AppEvent::KeyPress(id, e));
                window.set_onkeydown(Some(&js_function!(onkeydown.emit)));
                let onkeyup = cb.reform(move |e| AppEvent::KeyRelease(id, e));
                window.set_onkeyup(Some(&js_function!(onkeyup.emit)));
            }

            AppEvent::Leave(_) => {
                let window = window();
                window.set_onkeydown(None);
                window.set_onkeyup(None);
            }

            AppEvent::Undo(ref actions) => {
                for action in actions.iter() {
                    match *action {
                        EditorAction::Select {
                            from,
                            prev_selected_tab,
                            ..
                        } => {
                            ctx.editor.selected_block = from;
                            ctx.editor.selected_tab = prev_selected_tab;
                        }

                        EditorAction::SwitchTab { from, .. } => ctx.editor.selected_tab = from,

                        EditorAction::SetSnapStep { from, .. } => ctx.editor.snap_step = from,

                        _ => (),
                    }
                }
            }

            AppEvent::Redo(ref actions) => {
                for action in actions.iter() {
                    match *action {
                        EditorAction::Select { to, .. } => {
                            ctx.editor.selected_block = to;
                            ctx.editor.selected_tab = 0;
                        }

                        EditorAction::SwitchTab { to, .. } => ctx.editor.selected_tab = to,

                        EditorAction::SetSnapStep { to, .. } => ctx.editor.snap_step = to,

                        _ => (),
                    }
                }
            }

            _ => (),
        }

        self.forward_event(event, app)?
    }

    #[apply(fallible!)]
    pub fn render(&self, app: &AppContext) -> Html {
        // TODO: add switching between selected blocks
        let pattern = self.sequencer.pattern().get()?;
        let block = self
            .ctx
            .selected_block
            .try_map(|i| pattern.selection().get(i))?
            .try_map(|i| pattern.data().get(*i))?;
        let ctx = ContextRef {
            editor: &self.ctx,
            app,
        };
        let emitter = ctx.event_emitter();
        let special_action = self.ctx.special_action;

        html! {
            <>
                <div
                    id="main-panel"
                >
                    <div
                        id="ctrl-panel"
                        class="dark-bg"
                        data-main-hint="Settings"
                        data-aux-hint={block.as_ref().map_or("General".into(), |x| x.to_attr_value())}
                    >
                        <div
                            id="hint"
                            class="light-bg"
                            data-main-hint="Hint bar"
                            data-aux-hint="for useful messages about the app's controls"
                        >
                            <span id="main-hint" ref={self.hint_handler.main_bar()} />
                            <br />
                            <span id="aux-hint" ref={self.hint_handler.aux_bar()} />
                        </div>
                        if let Some(block) = block {
                            <div id="tab-list">{ block.tabs(ctx) }</div>
                            { block.sound.params(ctx, &self.sequencer) }
                            <div
                                id="general-ctrl"
                                class="dark-bg"
                            >
                                <Button
                                    name="Back to project-wide settings"
                                    onclick={emitter.reform(|_| AppEvent::Select(None))}
                                >
                                    <img::House />
                                </Button>
                                <Button
                                    name="Remove sound block"
                                    class="red-on-hover"
                                    onclick={emitter.reform(|_| AppEvent::Remove)}
                                >
                                    <img::Cross />
                                </Button>
                            </div>
                        } else {
                            <div id="tab-list">{ self.sequencer.tabs(ctx) }</div>
                            { self.sequencer.params(ctx) }
                        }
                    </div>
                    <GraphEditorCanvas<SoundBlock>
                        editor={self.sequencer.pattern()}
                        emitter={emitter.clone()}
                    />
                </div>
                <div
                    id="io-panel"
                    data-main-hint="Editor plane settings"
                >
                    <div
                        class="horizontal-menu"
                        id="actions"
                    >
                        { for ctx
                            .actions()
                            .iter()
                            .rev()
                            .enumerate()
                            .map(|(i, a)| self.render_action(a, i, emitter)) }
                    </div>
                    <div
                        id="special-actions"
                    >
                        <Button
                            name="Special Action: Select"
                            class={(special_action == SpecialAction::Select).choose("small selected", "small")}
                            help="Click to select points when pressing Meta in an editor space"
                            onclick={emitter
                                .reform(|_| AppEvent::SetSpecialAction(SpecialAction::Select))}
                        >
                            <img::Selection />
                        </Button>
                        <Button
                            name="Special Action: Add"
                            class={(special_action == SpecialAction::Add).choose("small selected", "small")}
                            help="Click to add points when pressing Meta in an editor space"
                            onclick={emitter
                                .reform(|_| AppEvent::SetSpecialAction(SpecialAction::Add))}
                        >
                            <img::Plus />
                        </Button>
                        <Button
                            name="Special Action: Remove"
                            class={(special_action == SpecialAction::Remove).choose("small selected", "small")}
                            help="Click to remove points when pressing Meta in an editor space"
                            onclick={emitter
                                .reform(|_| AppEvent::SetSpecialAction(SpecialAction::Remove))}
                        >
                            <img::Minus />
                        </Button>
                    </div>
                    <div
                        id="editor-settings"
                        data-main-hint="Editor settings"
                    >
                        <Switch
                            key="snap"
                            name="Interval for blocks to snap to"
                            setter={emitter.reform(|x: usize| {
                                AppEvent::SnapStep(
                                    *[r64!(0), r64!(1), r64!(0.5), r64!(0.25), r64!(0.125)]
                                        .get_wrapping(x)
                                )
                            })}
                            options={vec!["None", "1", "1/2", "1/4", "1/8"]}
                            initial={match *self.ctx.snap_step {
                                x if x == 1.0   => 1,
                                x if x == 0.5   => 2,
                                x if x == 0.25  => 3,
                                x if x == 0.125 => 4,
                                _ => 0,
                            }}
                        />
                    </div>
                    if self.sequencer.playback_ctx().all_playing() {
                        <Button
                            name="Stop"
                            onclick={emitter.reform(|_| AppEvent::StopPlay)}
                        >
                            <img::Stop />
                        </Button>
                    } else {
                        <Button
                            name="Play"
                            onclick={emitter.reform(|_| AppEvent::PreparePlay(None))}
                        >
                            <img::Play />
                        </Button>
                    }
                    <canvas
                        id="sound-visualiser"
                        ref={self.sound_visualiser.canvas()}
                        class="blue-border"
                        data-main-hint="Sound visualiser"
                    />
                </div>
            </>
        }
    }
}

impl Editor {
    #[apply(fallible!)]
    fn forward_event(&mut self, event: &mut AppEvent, app: &mut AppContext) {
        let mut ctx = ContextMut {
            editor: &mut self.ctx,
            app,
        };
        self.hint_handler.handle_event(event)?;
        self.sound_visualiser.handle_event(event, &self.sequencer)?;
        self.sequencer.handle_event(event, ctx.as_mut())?;
        let mut pattern = self.sequencer.pattern().get_mut()?;
        if let Some(&id) = pattern.selection().first() {
            let mut block = pattern.get_mut(id)?;
            let offset = block.offset;
            block
                .inner()
                .handle_event(event, ctx.as_mut(), &self.sequencer, offset)?;
        }
    }

    fn render_action(
        &self,
        action: &EditorAction,
        index: usize,
        emitter: &Callback<AppEvent>,
    ) -> Html {
        match index.cmp(&self.ctx.undid_actions) {
            Ordering::Less if let Some(name) = action.name() => {
                let index = self.ctx.undid_actions - index;
                html! {
                    <Button
                        {name}
                        class="undone"
                        help={match index {
                        1 => AttrValue::Static("Click to redo this action"),
                        2 => AttrValue::Static("Click to redo this and the previous action"),
                        _ => format!("Click to redo this and {index} previous actions").into()
                    }}
                        onclick={emitter.reform(move |_| AppEvent::Rewind(index))}
                    >
                        <s >{ name }</s>
                    </Button>
                }
            },

            Ordering::Equal => html! {
                if let Some(name) = action.name() {
                    <Button {name} class="selected" help="Last action"><p >{ name }</p></Button>
                }
            },

            Ordering::Greater if let Some(name) = action.name() => {
                let index = index - self.ctx.undid_actions;
                html! {
                    <Button
                        {name}
                        help={match index {
                        1 => AttrValue::Static("Click to undo the next action"),
                        _ => format!("Click to undo {index} subsequent actions").into()
                    }}
                        onclick={emitter.reform(move |_| AppEvent::Unwind(index))}
                    >
                        <p >{ name }</p>
                    </Button>
                }
            }

            _ => html!()
        }
    }
}
