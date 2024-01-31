use bevy::{
    render::texture::Image,
    ui::{self, UiImage},
};

use super::builder::{AssetPathParam, ColorParam, StyleBuilder};

#[allow(missing_docs)]
pub trait StyleBuilderBackground {
    /// Set the background image of the target entity.
    fn background_image<'p>(&mut self, path: impl AssetPathParam<'p>) -> &mut Self;

    /// Set the background image of the target entity, and also explicitly configure the
    /// horizontal and vertical flip.
    fn background_image_flipped<'p>(
        &mut self,
        path: impl AssetPathParam<'p>,
        flip_x: bool,
        flip_y: bool,
    ) -> &mut Self;

    /// Set the background color, or `None` for transparent.
    fn background_color(&mut self, color: impl ColorParam) -> &mut Self;
}

impl<'a, 'w> StyleBuilderBackground for StyleBuilder<'a, 'w> {
    fn background_image<'p>(&mut self, path: impl AssetPathParam<'p>) -> &mut Self {
        let texture = path.to_path().map(|p| self.load_asset::<Image>(p));
        match (texture, self.target.get_mut::<UiImage>()) {
            (Some(texture), Some(mut uii)) => {
                uii.texture = texture;
            }
            (Some(texture), None) => {
                self.target.insert(UiImage {
                    texture,
                    ..Default::default()
                });
            }
            (None, Some(_)) => {
                self.target.remove::<UiImage>();
            }
            _ => (),
        };
        self
    }

    fn background_image_flipped<'p>(
        &mut self,
        path: impl AssetPathParam<'p>,
        flip_x: bool,
        flip_y: bool,
    ) -> &mut Self {
        let texture = path.to_path().map(|p| self.load_asset::<Image>(p));
        match (texture, self.target.get_mut::<UiImage>()) {
            (Some(texture), Some(mut uii)) => {
                uii.texture = texture;
                uii.flip_x = flip_x;
                uii.flip_y = flip_y;
            }
            (Some(texture), None) => {
                self.target.insert(UiImage {
                    texture,
                    flip_x,
                    flip_y,
                });
            }
            (None, Some(_)) => {
                self.target.remove::<UiImage>();
            }
            _ => (),
        };
        self
    }

    fn background_color(&mut self, color: impl ColorParam) -> &mut Self {
        if let Some(color) = color.to_val() {
            self.target.insert(ui::BackgroundColor(color));
        } else {
            self.target.remove::<ui::BackgroundColor>();
        }
        self
    }
}
