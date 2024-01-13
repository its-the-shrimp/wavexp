use std::{mem::replace, rc::Rc};

use wavexp_utils::{cell::Shared, AppResult, AppResultUtils, BoolExt, OptionExt};
use web_sys::HtmlInputElement;
use yew::{html, AttrValue, Html, TargetCast};

use crate::{
    global::{AppAction, AppContext, AppEvent},
    img,
    input::{AudioInputButton, Button, Slider},
    sequencer::Sequencer,
    sound::{AudioInput, FromBeats},
};

/// Handles rendering of a pop-up window in the center of the screen.
#[derive(Debug, Clone, PartialEq)]
pub enum Popup {
    /// Choose the audio input for the selected sound block.
    ChooseInput,
    /// Edit the contained audio input.
    EditInput(Shared<AudioInput>),
    /// Save the sequence as a file.
    Export {
        filename: Rc<str>,
        err_msg: AttrValue,
    },
}

impl Popup {
    pub fn handle_event(&mut self, event: &AppEvent, ctx: &mut AppContext) -> AppResult<()> {
        Ok(match *event {
            AppEvent::SetOutputFileName(ref e) => {
                if let Self::Export { filename, err_msg } = self {
                    let to: Rc<str> = e
                        .target_dyn_into::<HtmlInputElement>()
                        .to_app_result()?
                        .value()
                        .into();
                    let from = replace(filename, to.clone());
                    *err_msg = "".into();
                    ctx.register_action(AppAction::SetOutputFileName { from, to });
                }
            }

            AppEvent::ExplainInvalidExportFileName(ref e) => {
                if let Self::Export { err_msg, .. } = self {
                    let value: Rc<str> = e
                        .target_dyn_into::<HtmlInputElement>()
                        .to_app_result()?
                        .value()
                        .into();
                    let mut parts = value.split('.');
                    let [base, ext] = [parts.next(), parts.next()];
                    // empty name case isn't matched as browser's indications should suffice then
                    if base == Some("") {
                        *err_msg = "Base name isn't provided".into();
                    } else if ext.is_none() {
                        *err_msg = "File extension not provided".into();
                    } else if ext != Some("wav") {
                        *err_msg =
                            format!("Invalid/unsupported format: {:?}", ext.unwrap_or("")).into();
                    }
                    ctx.force_rerender();
                }
            }

            AppEvent::SetInputName(ref e) => {
                if let Self::EditInput(input) = self {
                    let to: Rc<str> = e
                        .target_dyn_into::<HtmlInputElement>()
                        .to_app_result()?
                        .value()
                        .into();
                    if !to.is_empty() {
                        let from = input.get_mut()?.set_name(to.clone());
                        ctx.register_action(AppAction::SetInputName { from, to });
                    }
                }
            }

            AppEvent::ReverseInput => {
                if let Self::EditInput(input) = self {
                    input.get_mut()?.changes_mut().reversed.flip();
                    ctx.register_action(AppAction::ReverseInput);
                }
            }

            AppEvent::SetStartCutOff(to) => {
                if let Self::EditInput(input) = self {
                    let from = replace(&mut input.get_mut()?.changes_mut().cut_start, to);
                    ctx.register_action(AppAction::SetStartCutOff { from, to });
                }
            }

            AppEvent::SetEndCutOff(to) => {
                if let Self::EditInput(input) = self {
                    let from = replace(&mut input.get_mut()?.changes_mut().cut_end, to);
                    ctx.register_action(AppAction::SetEndCutOff { from, to });
                }
            }

            AppEvent::Undo(ref actions) => {
                for action in actions.iter() {
                    match action {
                        AppAction::SetOutputFileName { from, .. } => {
                            if let Self::Export { filename, .. } = self {
                                *filename = from.clone();
                            }
                        }

                        AppAction::SetInputName { from, .. } => {
                            if let Self::EditInput(input) = self {
                                input.get_mut()?.set_name(from.clone());
                            }
                        }

                        AppAction::ReverseInput => {
                            if let Self::EditInput(input) = self {
                                input.get_mut()?.changes_mut().reversed.flip();
                            }
                        }

                        AppAction::SetStartCutOff { from, .. } => {
                            if let Self::EditInput(input) = self {
                                input.get_mut()?.changes_mut().cut_start = *from;
                            }
                        }

                        AppAction::SetEndCutOff { from, .. } => {
                            if let Self::EditInput(input) = self {
                                input.get_mut()?.changes_mut().cut_end = *from;
                            }
                        }

                        _ => (),
                    }
                }
            }

            AppEvent::Redo(ref actions) => {
                for action in actions.iter() {
                    match action {
                        AppAction::SetOutputFileName { to, .. } => {
                            if let Self::Export { filename, .. } = self {
                                *filename = to.clone();
                            }
                        }

                        AppAction::SetInputName { to, .. } => {
                            if let Self::EditInput(input) = self {
                                input.get_mut()?.set_name(to.clone());
                            }
                        }

                        AppAction::ReverseInput => {
                            if let Self::EditInput(input) = self {
                                input.get_mut()?.changes_mut().reversed.flip();
                            }
                        }

                        AppAction::SetStartCutOff { to, .. } => {
                            if let Self::EditInput(input) = self {
                                input.get_mut()?.changes_mut().cut_start = *to;
                            }
                        }

                        AppAction::SetEndCutOff { to, .. } => {
                            if let Self::EditInput(input) = self {
                                input.get_mut()?.changes_mut().cut_end = *to;
                            }
                        }

                        _ => (),
                    }
                }
            }

            _ => (),
        })
    }

    pub fn render(&self, ctx: &AppContext, sequencer: &Sequencer) -> Html {
        let emitter = ctx.event_emitter();
        match self {
            Self::ChooseInput => html! {
                <form id="popup-bg" method="dialog" onsubmit={emitter.reform(|_| AppEvent::ClosePopup)}>
                    <p>{"Choose audio input"}</p>
                    <Button name="Close the pop-up" class="small red-on-hover" submit=true>
                        <img::Cross/>
                    </Button>
                    <div class="blue-border" data-main-hint="Choose audio input">
                        <div class="dark-bg horizontal-menu-wrapper full">
                            <div class="horizontal-menu dark-bg">
                                {for sequencer.inputs().iter().map(|input| html!{
                                    <AudioInputButton class="extend-inner-button-panel"
                                    bps={sequencer.bps()} {input} {emitter}
                                    onclick={{
                                        let i = input.clone();
                                        emitter.reform(move |_| AppEvent::SelectInput(i.clone()))
                                    }}
                                    playing={sequencer.playback_ctx().played_input()
                                        .is_some_and(|cur| cur.eq(input))}
                                    name={input.get().map_or_else(|_| "".into(), |x| x.name().clone())}/>
                                })}
                                <Button name="Add audio input"
                                onclick={emitter.reform(|_| AppEvent::StartInputAdd)}>
                                    <img::Plus/>
                                </Button>
                            </div>
                        </div>
                    </div>
                </form>
            },

            Self::EditInput(input_outer) => html! {
                <form id="popup-bg" method="dialog" onsubmit={emitter.reform(|_| AppEvent::ClosePopup)}>
                    <p>{"Edit audio input"}</p>
                    <Button name="Close the pop-up" class="small red-on-hover" submit=true>
                        <img::Cross/>
                    </Button>
                    <div class="dark-bg blue-border" data-main-hint="Edit audio input">
                        <div id="popup-core">
                            if let Some(input) = input_outer.get().report() {
                                <div style="display: grid; grid-template-columns: repeat(3, 1fr)">
                                    if input.changes().reversed {
                                        <Button name="Playback direction: reverse" class="small"
                                        help="Click to reverse the audio input"
                                        onclick={emitter.reform(|_| AppEvent::ReverseInput)}>
                                            <img::LeftArrow/>
                                        </Button>
                                    } else {
                                        <Button name="Playback direction: normal" class="small"
                                        help="Click to reverse the audio input"
                                        onclick={emitter.reform(|_| AppEvent::ReverseInput)}>
                                            <img::RightArrow/>
                                        </Button>
                                    }
                                    <input type="text" value={input.name().clone()}
                                    placeholder="Enter name..." required=true
                                    class="dark-bg blue-border" data-main-hint="Audio input name"
                                    onchange={emitter.reform(AppEvent::SetInputName)}/>
                                </div>
                                <div style="display: grid; grid-template-columns: repeat(2, 1fr)">
                                    <Slider name="Start cut-off"
                                    max={input.raw_duration().secs_to_beats(sequencer.bps())}
                                    initial={input.changes().cut_start}
                                    setter={emitter.reform(AppEvent::SetStartCutOff)}/>
                                    <Slider name="End cut-off"
                                    max={input.raw_duration().secs_to_beats(sequencer.bps())}
                                    initial={input.changes().cut_end}
                                    setter={emitter.reform(AppEvent::SetEndCutOff)}/>
                                </div>
                            } else {
                                <p style="color:red">{"Failed to access the audio input"}</p>
                            }
                        </div>
                    </div>
                </form>
            },

            Self::Export { filename, err_msg } => html! {
                <form id="popup-bg" method="dialog" onsubmit={{
                    let filename = filename.clone();
                    emitter.reform(move |_| AppEvent::PrepareExport(filename.clone()))
                }}>
                    <p>{"Export the project"}</p>
                    <Button name="Close the pop-up" class="small red-on-hover"
                    onclick={emitter.reform(|_| AppEvent::ClosePopup)}>
                        <img::Cross/>
                    </Button>
                    <div class="dark-bg blue-border" data-main-hint="Export the project">
                        <div id="popup-core">
                            <input type="text" value={filename.clone()} pattern=".*\\.wav"
                            placeholder="Enter file name..." required=true
                            class="dark-bg blue-border" data-main-hint="Output file name"
                            oninvalid={emitter.reform(AppEvent::ExplainInvalidExportFileName)}
                            onchange={emitter.reform(AppEvent::SetOutputFileName)}/>
                            <Button name="Save" class="wide" submit=true>
                                <p>{"Save"}</p>
                            </Button>
                        </div>
                    </div>
                    if !err_msg.is_empty() {
                        <p class="error">{"Error: "}{err_msg}</p>
                    }
                </form>
            },
        }
    }
}
