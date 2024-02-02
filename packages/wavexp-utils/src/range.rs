use std::{
    cmp::Ordering,
    ops::{Bound, Range, RangeBounds, RangeInclusive, Residual, Try},
};

pub trait RangeBoundsExt<T>: Sized + RangeBounds<T> {
    type HKT<Bound>;

    fn into_bounds(self) -> (Bound<T>, Bound<T>);

    /// if `value` is outside of `self`, extend `self` just enough for `value` to be inside it
    fn extend<R>(self, value: R) -> Self::HKT<R>
    where
        T: PartialOrd<R> + Into<R>;

    /// turns `x .. y` into `f(x) .. f(y)`
    fn map_bounds<R>(self, f: impl FnMut(T) -> R) -> Self::HKT<R>;

    /// turns `x .. y` into `f(x)? .. f(y)?`
    fn try_map_bounds<R>(
        self,
        f: impl FnMut(T) -> R,
    ) -> <R::Residual as Residual<Self::HKT<R::Output>>>::TryType
    where
        R: Try,
        R::Residual: Residual<Self::HKT<R::Output>>;

    fn fit(self, item: T) -> T
    where
        T: Ord,
    {
        use Bound::{Excluded as X, Included as I, Unbounded as U};
        match self.into_bounds() {
            (I(min) | X(min), I(max) | X(max)) => item.clamp(min, max),
            (I(min) | X(min), U) => item.max(min),
            (U, I(max) | X(max)) => item.min(max),
            (U, U) => item,
        }
    }

    /// Returns a boolean indicating whether `self` & `other` overlap.
    #[rustfmt::skip]
    fn overlap<U>(&self, other: &impl RangeBounds<U>) -> bool
    where
        U: PartialOrd<T>,
        T: PartialOrd<U>,
    {
        // This impl is based on the fact that overlap detection is commutative and that exclusion
        // propagates, i.e. if at least 1 bound, of the 2 that are compared, is exclusive, then the
        // whole comparison is exclusive
        use Bound::{Excluded as X, Included as I, Unbounded as U};
        match (self.start_bound(), self.end_bound(), other.start_bound(), other.end_bound()) {
            // all inclusive
            (I(start), I(end), I(other_start), I(other_end))
            => start <= other_end && other_start <= end,

            // outer exclusive, inner inclusive
            | (I(start), I(end), I(other_start), X(other_end))
            | (X(start), I(end), I(other_start), I(other_end))
            | (X(start), I(end), I(other_start), X(other_end))
            => start < other_end && other_start <= end,

            // outer inclusive, inner exclusive
            | (I(start), I(end), X(other_start), I(other_end))
            | (I(start), X(end), I(other_start), I(other_end))
            | (I(start), X(end), X(other_start), I(other_end))
            => start <= other_end && other_start < end,

            // all exclusive
            | (I(start), I(end), X(other_start), X(other_end))
            | (X(start), X(end), I(other_start), I(other_end))
            | (I(start), X(end), I(other_start), X(other_end))
            | (X(start), I(end), X(other_start), I(other_end))
            | (I(start), X(end), X(other_start), X(other_end))
            | (X(start), I(end), X(other_start), X(other_end))
            | (X(start), X(end), I(other_start), X(other_end))
            | (X(start), X(end), X(other_start), I(other_end))
            | (X(start), X(end), X(other_start), X(other_end))
            => start < other_end && other_start < end,

            // outer unbounded, inner inclusive
            (U, I(end), I(other_start), _) => other_start <= end,
            (_, I(end), I(other_start), U) => other_start <= end,

            // outer unbounded, inner exclusive
            | (U, X(end), I(other_start), _)
            | (U, I(end), X(other_start), _)
            | (U, X(end), X(other_start), _)
            => other_start < end,
            | (_, X(end), I(other_start), U)
            | (_, I(end), X(other_start), U)
            | (_, X(end), X(other_start), U)
            => other_start < end,

            // outer inclusive, inner unbounded
            (I(start), U, _, I(other_end)) => start <= other_end,
            (I(start), _, U, I(other_end)) => start <= other_end,

            // outer exclusive, inner unbounded
            | (X(start), U, _, I(other_end))
            | (I(start), U, _, X(other_end))
            | (X(start), U, _, X(other_end))
            => start < other_end,
            | (X(start), _, U, I(other_end))
            | (I(start), _, U, X(other_end))
            | (X(start), _, U, X(other_end))
            => start < other_end,

            // at least one of the ranges fully unbounded or both unbounded on the same side
            (U, U, _, _) | (_, _, U, U) | (U, _, U, _) | (_, U, _, U) => true
        }
    }
}

macro_rules! impl_range_bounds_ext_for_range {
    ($r:ident) => {
        impl<T> RangeBoundsExt<T> for $r<T> {
            type HKT<Bound> = $r<Bound>;

            fn into_bounds(self) -> (Bound<T>, Bound<T>) {
                (Bound::Included(self.start), Bound::Excluded(self.end))
            }

            fn extend<R>(self, value: R) -> Self::HKT<R>
            where
                T: PartialOrd<R> + Into<R>,
            {
                if self.start > value {
                    $r {
                        start: value,
                        end: self.end.into(),
                    }
                } else if self.end <= value {
                    $r {
                        start: self.start.into(),
                        end: value,
                    }
                } else {
                    self.map_bounds(Into::into)
                }
            }

            fn map_bounds<R>(self, mut f: impl FnMut(T) -> R) -> Self::HKT<R> {
                $r {
                    start: f(self.start),
                    end: f(self.end),
                }
            }

            fn try_map_bounds<R>(
                self,
                mut f: impl FnMut(T) -> R,
            ) -> <R::Residual as Residual<Self::HKT<R::Output>>>::TryType
            where
                R: Try,
                R::Residual: Residual<Self::HKT<R::Output>>,
            {
                try {
                    $r {
                        start: f(self.start)?,
                        end: f(self.end)?,
                    }
                }
            }
        }
    };
}

impl_range_bounds_ext_for_range!(Range);

macro_rules! impl_range_bounds_ext_for_range_inclusive {
    ($r:ident) => {
        impl<T> RangeBoundsExt<T> for $r<T> {
            type HKT<R> = $r<R>;

            fn into_bounds(self) -> (Bound<T>, Bound<T>) {
                let (start, end) = self.into_inner();
                (Bound::Included(start), Bound::Included(end))
            }

            fn extend<R>(self, value: R) -> Self::HKT<R>
            where
                T: PartialOrd<R> + Into<R>,
            {
                let (start, end) = self.into_inner();
                if start > value {
                    Self::HKT::new(value, end.into())
                } else if end < value {
                    Self::HKT::new(start.into(), value)
                } else {
                    Self::HKT::new(start.into(), end.into())
                }
            }

            fn map_bounds<R>(self, mut f: impl FnMut(T) -> R) -> Self::HKT<R> {
                let (start, end) = self.into_inner();
                Self::HKT::new(f(start), f(end))
            }

            fn try_map_bounds<R>(
                self,
                mut f: impl FnMut(T) -> R,
            ) -> <R::Residual as Residual<Self::HKT<R::Output>>>::TryType
            where
                R: Try,
                R::Residual: Residual<Self::HKT<R::Output>>,
            {
                let (start, end) = self.into_inner();
                try { Self::HKT::new(f(start)?, f(end)?) }
            }
        }
    };
}

impl_range_bounds_ext_for_range_inclusive!(RangeInclusive);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RangeV2<T> {
    pub start: T,
    pub end: T,
}

impl<T> RangeBounds<T> for RangeV2<T> {
    #[inline]
    fn start_bound(&self) -> Bound<&T> {
        Bound::Included(&self.start)
    }

    #[inline]
    fn end_bound(&self) -> Bound<&T> {
        Bound::Excluded(&self.end)
    }
}

impl_range_bounds_ext_for_range!(RangeV2);

impl<T> RangeV2<T> {
    pub const fn unit(x: T) -> Self
    where
        T: Copy,
    {
        Self { start: x, end: x }
    }

    pub fn is_empty(&self) -> bool
    where
        T: PartialOrd,
    {
        self.start
            .partial_cmp(&self.end)
            .map_or(true, Ordering::is_ge)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RangeInclusiveV2<T> {
    pub start: T,
    pub end: T,
}

impl<T> RangeBounds<T> for RangeInclusiveV2<T> {
    #[inline]
    fn start_bound(&self) -> Bound<&T> {
        Bound::Included(&self.start)
    }

    #[inline]
    fn end_bound(&self) -> Bound<&T> {
        Bound::Included(&self.end)
    }
}

impl_range_bounds_ext_for_range_inclusive!(RangeInclusiveV2);

impl<T> RangeInclusiveV2<T> {
    pub const fn new(start: T, end: T) -> Self {
        Self { start, end }
    }

    pub fn into_inner(self) -> (T, T) {
        (self.start, self.end)
    }

    pub fn is_empty(&self) -> bool
    where
        T: PartialOrd,
    {
        self.start
            .partial_cmp(&self.end)
            .map_or(true, Ordering::is_gt)
    }
}

pub trait IntoRange: Sized {
    fn range_to(self, end: Self) -> RangeV2<Self>;
    fn incl_range_to(self, end: Self) -> RangeInclusiveV2<Self>;
    fn sorted_range_to(self, end: Self) -> RangeV2<Self>
    where
        Self: Ord;
    fn sorted_incl_range_to(self, end: Self) -> RangeInclusiveV2<Self>
    where
        Self: Ord;
}

impl<T> IntoRange for T {
    fn range_to(self, end: Self) -> RangeV2<Self> {
        RangeV2 { start: self, end }
    }

    fn incl_range_to(self, end: Self) -> RangeInclusiveV2<Self> {
        RangeInclusiveV2 { start: self, end }
    }

    fn sorted_range_to(self, end: Self) -> RangeV2<Self>
    where
        Self: Ord,
    {
        if self > end {
            RangeV2 {
                start: end,
                end: self,
            }
        } else {
            RangeV2 { start: self, end }
        }
    }

    fn sorted_incl_range_to(self, end: Self) -> RangeInclusiveV2<Self>
    where
        Self: Ord,
    {
        if self > end {
            RangeInclusiveV2 {
                start: end,
                end: self,
            }
        } else {
            RangeInclusiveV2 { start: self, end }
        }
    }
}

pub trait FitInto: Ord + Sized {
    fn fit_into(self, range: impl RangeBoundsExt<Self>) -> Self {
        range.fit(self)
    }
}

impl<T: Ord> FitInto for T {}
