use js_sys::Function;
use wasm_bindgen::JsCast;
use wavexp_utils::{
    js_function, now, r64, window, AppResult, AppResultUtils, OptionExt, Take, R64,
};
use yew::{html, html::Context, Callback, Component, Html};

use crate::{
    ctx::{AppEvent, ContextMut, EditorAction},
    editor::Editor,
    img,
    popup::Popup,
    sound::Secs,
};

/// carries all the app-wide settings that are passed to all the event receivers
pub struct AppContext {
    frame: Secs,
    event_emitter: Callback<AppEvent>,
    rerender_needed: bool,
}

impl AppContext {
    pub fn new(event_emitter: Callback<AppEvent>) -> AppResult<Self> {
        Ok(Self {
            frame: now().to_app_result()? / 1000,
            rerender_needed: false,
            event_emitter,
        })
    }

    pub fn force_rerender(&mut self) {
        self.rerender_needed = true
    }
}

impl ContextMut<'_, '_> {
    pub fn frame(&self) -> Secs {
        self.app.frame
    }

    pub fn event_emitter(&self) -> &Callback<AppEvent> {
        &self.app.event_emitter
    }

    pub fn emit_event(&self, event: AppEvent) {
        self.app.event_emitter.emit(event)
    }
}

impl ContextMut<'_, '_> {
    pub fn force_rerender(&mut self) {
        self.app.rerender_needed = true
    }
}

pub struct App {
    projects: Vec<Editor>,
    selected_proj: usize,
    ctx: AppContext,
    frame_emitter: Function,
    /// pop-ups are stacked on each other if one is opened from within another one.
    popups: Vec<Popup>,
}

impl Component for App {
    type Message = AppEvent;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let cb = ctx.link().callback(AppEvent::Frame);
        let res = Self {
            projects: vec![Editor::new()],
            selected_proj: 0,
            ctx: AppContext::new(ctx.link().callback(|x| x)).unwrap(),
            frame_emitter: js_function!(|x| cb.emit(R64::new_or(r64![0], x))),
            popups: vec![],
        };
        window()
            .request_animation_frame(&res.frame_emitter)
            .unwrap();
        res
    }

    fn update(&mut self, _: &Context<Self>, mut msg: Self::Message) -> bool {
        let res: AppResult<_> = try {
            let selected_proj = self.projects.get_mut(self.selected_proj).to_app_result()?;
            match msg {
                AppEvent::Frame(frame) => {
                    window().request_animation_frame(&self.frame_emitter)?;
                    self.ctx.frame = frame / 1000
                }

                AppEvent::StartPlay(_) | AppEvent::StopPlay => self.ctx.rerender_needed = true,

                AppEvent::KeyPress(_, ref e) if !e.repeat() && e.code() == "Escape" => {
                    if let Some(closed) = self.popups.pop() {
                        e.prevent_default();
                        selected_proj
                            .ctx
                            .register_action(&mut self.ctx, EditorAction::ClosePopup(closed));
                    }
                }

                AppEvent::OpenPopup(ref opened) => {
                    selected_proj
                        .ctx
                        .register_action(&mut self.ctx, EditorAction::OpenPopup(opened.clone()));
                    self.popups.push(opened.clone());
                }

                AppEvent::ClosePopup => {
                    let closed = self.popups.pop().to_app_result()?;
                    selected_proj
                        .ctx
                        .register_action(&mut self.ctx, EditorAction::ClosePopup(closed));
                }

                AppEvent::Undo(ref actions) => {
                    for action in actions.iter() {
                        match *action {
                            EditorAction::OpenPopup(_) => _ = self.popups.pop(),

                            EditorAction::ClosePopup(ref popup) => self.popups.push(popup.clone()),

                            _ => (),
                        }
                    }
                }

                AppEvent::Redo(ref actions) => {
                    for action in actions.iter() {
                        match *action {
                            EditorAction::OpenPopup(ref popup) => self.popups.push(popup.clone()),

                            EditorAction::ClosePopup(_) => _ = self.popups.pop(),

                            _ => (),
                        }
                    }
                }

                _ => (),
            }
            selected_proj.handle_event(&mut msg, &mut self.ctx)?;
            if let Some(popup) = self.popups.last_mut() {
                popup.handle_event(
                    &msg,
                    ContextMut {
                        app: &mut self.ctx,
                        editor: &mut selected_proj.ctx,
                    },
                )?;
            }
            self.ctx.rerender_needed.take()
        };
        res.report().unwrap_or(false)
    }

    fn view(&self, _: &Context<Self>) -> Html {
        // TODO: no panics
        let project = &self.projects[self.selected_proj];
        let sequencer = project.sequencer.get().unwrap();
        html! {
            <>
                if let Some(popup) = self.popups.last() {
                    { popup.render(&self.ctx.event_emitter, &sequencer) }
                }
                { project.render(&self.ctx) }
                // TODO: add a loading/auto-save indicator
                <div
                    id="error-sign"
                    hidden=true
                    data-main-hint="Error has occured"
                    data-aux-hint="Check the console for more info"
                ><img::Warning /></div>
            </>
        }
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        if !first_render {
            return;
        }
        let window = window();

        let cb = ctx.link().callback(|_| AppEvent::Resize);
        window.set_onresize(Some(&js_function!(|| cb.emit(()))));

        let cb = ctx.link().callback(AppEvent::FetchHint);
        window.set_onpointerover(Some(&js_function!(cb.emit)));

        ctx.link().send_message(AppEvent::Resize);
    }
}
