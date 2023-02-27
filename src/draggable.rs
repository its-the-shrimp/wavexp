use crate::MainCmd;
use crate::utils;
use yew::TargetCast;
use std::rc::Rc;

#[derive(Debug)]
pub enum Cmd {
    Drag(web_sys::PointerEvent),
    Focus(web_sys::PointerEvent),
    Unfocus(web_sys::PointerEvent),
    HoverIn(web_sys::PointerEvent),
    HoverOut(web_sys::PointerEvent)
}

impl Cmd {

    pub fn handle_focus(self, f: impl FnOnce(&web_sys::PointerEvent) -> utils::JsResult<()>) -> utils::JsResult<Self> {
        if let Cmd::Focus(ref e) = self {
            e.target_unchecked_into::<web_sys::HtmlElement>()
                .set_pointer_capture(e.pointer_id())?;
            f(e)?;
        }
        Ok(self)
    }

    pub fn handle_unfocus(self, f: impl FnOnce(&web_sys::PointerEvent) -> utils::JsResult<()>) -> utils::JsResult<Self> {
        if let Cmd::Unfocus(ref e) = self {
            e.target_unchecked_into::<web_sys::HtmlElement>()
                .release_pointer_capture(e.pointer_id())?;
            f(e)?;
        }
        Ok(self)
    }

    pub fn handle_hover(self, help_msg: Rc<str>) -> Self {
        match &self {
            Cmd::HoverIn(_) => MainCmd::SetDesc(help_msg).send(),
            Cmd::HoverOut(_) => MainCmd::RemoveDesc.send(),
            _ => ()};
        self
    }

    pub fn handle_drag(self, f: impl FnOnce(&web_sys::PointerEvent) -> utils::JsResult<()>) -> utils::JsResult<Self> {
        if let Cmd::Drag(ref e) = self {
            f(e)?;
            e.stop_propagation();
            e.stop_immediate_propagation();
        };
        Ok(self)
    }

    #[inline] pub fn needs_rerender(self) -> bool {
        std::matches!(self, Cmd::Focus(_) | Cmd::Unfocus(_) | Cmd::Drag(_))
    }
}
