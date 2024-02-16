use yew::{function_component, html, Html};

#[function_component]
pub fn Settings() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon points="15,20 30,15 30,10 40,15 85,20 40,25 30,30 30,25" />
            <polygon points="85,50 70,45 70,40 60,45 15,50 60,55 70,60 70,55" />
            <polygon points="15,80 30,85 30,90 40,85 85,80 40,75 30,70 30,75" />
        </svg>
    }
}

#[function_component]
pub fn Play() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon points="25,25 75,50 25,75" />
        </svg>
    }
}

#[function_component]
pub fn Stop() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon points="25,25 75,25 75,75 25,75" />
        </svg>
    }
}

#[function_component]
pub fn Plus() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon
                points="
                40,10 60,10 60,40 90,40 90,60 60,60
                60,90 40,90 40,60 10,60 10,40 40,40
            "
            />
        </svg>
    }
}

#[function_component]
pub fn Minus() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <rect x=10 y=40 width=80 height=20 />
        </svg>
    }
}

#[function_component]
pub fn Warning() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon points="10,90 50,10 90,90" />
            <polygon points="48,40 52,40 52,60 48,60" />
            <polygon points="48,70 52,70 52,74 48,74" />
        </svg>
    }
}

#[function_component]
pub fn Cross() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon
                points="
                27,35 35,27 50,42 65,27 73,35 58,50
                73,65 65,73 50,58 35,73 27,65 42,50
            "
            />
        </svg>
    }
}

#[function_component]
pub fn House() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon points="20,60 50,20 80,60 70,60 70,80 30,80 30,60" />
        </svg>
    }
}

#[function_component]
pub fn RightArrow() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon points="50,15 85,50 50,85 50,70 15,70 15,30 50,30" />
        </svg>
    }
}

#[function_component]
pub fn LeftArrow() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon points="50,15 15,50 50,85 50,70 85,70 85,30 50,30" />
        </svg>
    }
}

#[function_component]
pub fn Selection() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <rect
                x=15
                y=15
                width=70
                height=70
                fill="transparent"
                stroke-width=5
                stroke-dasharray="15 10"
            />
        </svg>
    }
}

#[function_component]
pub fn FloppyDisk() -> Html {
    html! {
        <svg viewBox="0 0 100 100">
            <polygon points="40,15 80,15 80,85 20,85 20,30" />
        </svg>
    }
}
