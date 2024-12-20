use bevy::prelude::*;
use bevy_mod_stylebuilder::*;
use bevy_reactor_builder::{EntityStyleBuilder, UiBuilder, UiTemplate};
use bevy_reactor_signals::{IntoSignal, Signal};

use crate::colors;

/// Control that displays an icon.
#[derive(Clone)]
pub struct Icon {
    /// Asset path for the icon
    pub icon: HandleOrOwnedPath<Image>,

    /// Size of the icon in pixels.
    pub size: Vec2,

    /// Color of the icon.
    pub color: Signal<Color>,

    /// Additional styles to apply to the icon
    pub style: StyleHandle,
}

impl Icon {
    /// Create a new `Icon` from a `&str` or `Handle<Image>`.
    pub fn new(icon: impl Into<HandleOrOwnedPath<Image>>) -> Self {
        Self {
            icon: icon.into(),
            ..default()
        }
    }

    /// Set the size of the icon.
    pub fn size(mut self, size: Vec2) -> Self {
        self.size = size;
        self
    }

    /// Set the color of the icon.
    pub fn color(mut self, color: impl IntoSignal<Color>) -> Self {
        self.color = color.into_signal();
        self
    }

    /// Set the style of the icon.
    pub fn style<S: StyleTuple + 'static>(mut self, style: S) -> Self {
        self.style = style.into_handle();
        self
    }
}

impl Default for Icon {
    fn default() -> Self {
        Self {
            icon: HandleOrOwnedPath::default(),
            size: Vec2::splat(12.0),
            color: Signal::Constant(colors::FOREGROUND.into()),
            style: StyleHandle::default(),
        }
    }
}

impl UiTemplate for Icon {
    fn build(&self, builder: &mut UiBuilder) {
        let icon = self.icon.clone();
        let size = self.size;
        let color = self.color;

        builder
            .spawn(Node { ..default() })
            .styles((
                move |sb: &mut StyleBuilder| {
                    sb.width(size.x).height(size.y).background_image(&icon);
                },
                self.style.clone(),
            ))
            .style_dyn(
                move |rcx| color.get(rcx),
                |color, sb| {
                    sb.background_image_color(color);
                },
            );
    }
}
