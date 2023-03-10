use crate::MainCmd;
use crate::utils;
use yew::TargetCast;

#[derive(Debug)]
pub enum Cmd {
    Drag(web_sys::PointerEvent),
    Focus(web_sys::PointerEvent),
    Unfocus(web_sys::PointerEvent),
    HoverIn(web_sys::PointerEvent),
    HoverOut(web_sys::PointerEvent)
}

impl Cmd {

    pub fn handle_focus<F>(self, f: F) -> utils::JsResult<Self>
    where F: FnOnce(&web_sys::PointerEvent) -> utils::JsResult<()> {
        if let Cmd::Focus(e) = &self {
            e.target_unchecked_into::<web_sys::HtmlElement>()
                .set_pointer_capture(e.pointer_id())?;
            f(e)?;
        }
        Ok(self)
    }

    pub fn handle_unfocus<F>(self, f: F) -> utils::JsResult<Self>
    where F: FnOnce(&web_sys::PointerEvent) -> utils::JsResult<()> {
        if let Cmd::Unfocus(e) = &self {
            e.target_unchecked_into::<web_sys::HtmlElement>()
                .release_pointer_capture(e.pointer_id())?;
            f(e)?;
        }
        Ok(self)
    }

    // the boolean passed to `f` indicates whether the element is hovered over
    #[inline]
    pub fn handle_hover<F>(self, help_msg: &str, f: F) -> utils::JsResult<Self>
    where F: FnOnce(&web_sys::PointerEvent, bool) -> utils::JsResult<()> {
        match &self {
            Cmd::HoverIn(e) => {
                f(e, true)?;
                MainCmd::SetDesc(help_msg.to_owned()).send()}
            Cmd::HoverOut(e) => {
                f(e, false)?;
                MainCmd::RemoveDesc.send()}
            _ => ()};
        Ok(self)
    }

    pub fn handle_drag(self, f: impl FnOnce(&web_sys::PointerEvent) -> utils::JsResult<()>) -> utils::JsResult<Self> {
        if let Cmd::Drag(e) = &self {
            f(e)?;
        };
        Ok(self)
    }
}
