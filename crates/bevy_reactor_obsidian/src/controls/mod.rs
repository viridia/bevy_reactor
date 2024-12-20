mod barrier;
mod button;
mod checkbox;
mod core_slider;
mod dialog;
mod disabled;
mod disclosure_toggle;
mod gradient_slider;
mod icon;
mod icon_button;
mod scrollview;
mod slider;
mod spacer;
mod spinbox;
mod splitter;
mod swatch;
mod swatch_grid;
mod toggle_state;
mod tool_palette;

use bevy::app::Plugin;
pub use button::{Button, ButtonVariant};
pub use checkbox::Checkbox;
pub use core_slider::CoreSlider;
pub use dialog::{Dialog, DialogBody, DialogFooter, DialogHeader};
pub use disabled::{Disabled, IsDisabled};
pub use disclosure_toggle::DisclosureToggle;
pub use gradient_slider::{ColorGradient, GradientSlider};
pub use icon::Icon;
pub use icon_button::IconButton;
pub use scrollview::ScrollView;
pub use slider::Slider;
pub use spacer::Spacer;
pub use spinbox::SpinBox;
pub use splitter::{Splitter, SplitterDirection};
pub use swatch::Swatch;
pub use swatch_grid::SwatchGrid;
pub use tool_palette::{ToolButton, ToolPalette};

pub(crate) struct ControlEventsPlugin;

impl Plugin for ControlEventsPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        app.add_observer(toggle_state::toggle_on_key_input)
            .add_observer(toggle_state::toggle_on_pointer_click)
            .add_observer(button::button_on_key_event)
            .add_observer(button::button_on_pointer_down)
            .add_observer(button::button_on_pointer_up)
            .add_observer(button::button_on_pointer_click)
            .add_observer(button::button_on_pointer_drag_end)
            .add_observer(button::button_on_pointer_cancel)
            .add_observer(barrier::barrier_on_key_input)
            .add_observer(barrier::barrier_on_pointer_down)
            .add_observer(core_slider::slider_on_drag_start)
            .add_observer(core_slider::slider_on_drag_end)
            .add_observer(core_slider::slider_on_drag);
    }
}
