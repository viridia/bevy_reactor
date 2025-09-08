use crate::{Derived, Mutable, derived::ReadDerived, mutable::ReadMutable};

/// What type of reactive node underlies this signal. "Signals" in this framework represent
/// any kind of reactive data source, including mutable variables, derived signals, and memoized
/// computations.
#[derive(Copy, Debug)]
pub enum Signal<T> {
    /// A mutable variable that can be read and written to.
    Mutable(Mutable<T>),

    /// A readonly value that is computed from other signals.
    Derived(Derived<T>),

    /// A constant value, mainly useful for establishing defaults.
    Constant(T),
}

impl<T> Clone for Signal<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Signal::Mutable(mutable) => Signal::Mutable(*mutable),
            Signal::Derived(derived) => Signal::Derived(*derived),
            Signal::Constant(value) => Signal::Constant(value.clone()),
        }
    }
}

impl<T> Signal<T>
where
    T: Copy + Send + Sync + 'static,
{
    /// Read the value of the signal using Copy semantics.
    pub fn get<R: ReadMutable + ReadDerived>(&self, rc: &R) -> T {
        match self {
            Signal::Mutable(mutable) => rc.read_mutable(mutable),
            Signal::Derived(derived) => rc.read_derived(derived),
            Signal::Constant(value) => *value,
        }
    }
}

impl<T> Signal<T>
where
    T: Clone + Send + Sync + 'static,
{
    /// Read the value of the signal using Copy semantics.
    pub fn get_clone<R: ReadMutable + ReadDerived>(&self, rc: &R) -> T {
        match self {
            Signal::Mutable(mutable) => rc.read_mutable_clone(mutable),
            Signal::Derived(derived) => rc.read_derived_clone(derived),
            Signal::Constant(value) => value.clone(),
        }
    }
}

impl<T> Signal<T>
where
    T: Send + Sync + 'static,
{
    /// Read the value of the signal using a mapping function.
    pub fn map<R: ReadMutable + ReadDerived, U, F: Fn(&T) -> U>(&self, rc: &R, f: F) -> U {
        match self {
            Signal::Mutable(mutable) => rc.read_mutable_map(mutable, f),
            Signal::Derived(derived) => rc.read_derived_map(derived, f),
            Signal::Constant(value) => f(value),
        }
    }
}

/// Implement default if T has a default.
impl<T> Default for Signal<T>
where
    T: Default + Send + Sync + 'static,
{
    fn default() -> Self {
        Self::Constant(Default::default())
    }
}

/// Trait that defines values that can be converted into a `Signal`.
pub trait IntoSignal<T> {
    /// Convert the value into a `Signal`. For most types, this will be a `Signal::Constant`.
    /// For `Mutable` and `Derived` signals, this will be a `Signal::Mutable` or `Signal::Derived`
    fn into_signal(self) -> Signal<T>;
}

impl<T> IntoSignal<T> for T {
    fn into_signal(self) -> Signal<T> {
        Signal::Constant(self)
    }
}

impl<T> IntoSignal<T> for Mutable<T> {
    fn into_signal(self) -> Signal<T> {
        Signal::Mutable(self)
    }
}

impl<T> IntoSignal<T> for Derived<T> {
    fn into_signal(self) -> Signal<T> {
        Signal::Derived(self)
    }
}

impl<T> IntoSignal<T> for Signal<T> {
    fn into_signal(self) -> Signal<T> {
        self
    }
}
