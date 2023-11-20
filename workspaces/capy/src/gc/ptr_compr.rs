#[cfg(feature = "compressed-oops")]
pub mod enabled {
    use std::sync::atomic::AtomicUsize;

    use crate::runtime::{utils::round_down_usize, value::Word, PTR_COMPR_BASE_ALIGNMENT};

    /// This is just a collection of compression scheme related functions.
    pub struct HeapCompressionScheme;

    static BASE: AtomicUsize = AtomicUsize::new(0);

    impl HeapCompressionScheme {
        pub(crate) fn init_base(base: usize) {
            BASE.store(base, std::sync::atomic::Ordering::Relaxed);
        }

        #[inline(always)]
        pub fn base() -> usize {
            BASE.load(std::sync::atomic::Ordering::Relaxed)
        }

        #[inline(always)]
        pub fn get_ptr_compr_base_address(base: usize) -> usize {
            round_down_usize(base, PTR_COMPR_BASE_ALIGNMENT)
        }

        #[inline(always)]
        pub fn compress_object(tagged: usize) -> u32 {
            tagged as u32
        }

        #[inline(always)]
        pub fn decompress_tagged_signed(raw_value: Word) -> usize {
            raw_value as usize
        }

        #[inline(always)]
        pub fn decompress_tagged_fast(on_heap_address: usize, raw_value: Word) -> usize {
            let base = Self::get_ptr_compr_base_address(on_heap_address);
            let result = base + raw_value as usize;
            debug_assert_eq!(
                result as u32, raw_value,
                "on_heap_address does not seem to point to heap base: {:x} != {:x}",
                on_heap_address,
                Self::base()
            );
            result
        }

        #[inline(always)]
        pub fn decompress_tagged(raw_value: Word) -> usize {
            Self::base() + raw_value as usize
        }
    }
}

#[cfg(not(feature = "compressed-oops"))]
pub mod enabled {
    use crate::runtime::value::Word;
    pub struct HeapCompressionScheme;

    impl HeapCompressionScheme {
        pub(crate) fn init_base(_ : usize) {
            unreachable!()
        }

        pub fn base() -> usize {
            unreachable!()
        }

        pub fn get_ptr_compr_base_address(_: usize) -> usize {
            unreachable!()
        }

        #[inline(always)]
        pub fn compress_object(_: usize) -> u32 {
            unreachable!()
        }

        #[inline(always)]
        pub fn decompress_tagged_signed(_: Word) -> usize {
            unreachable!()
        }

        #[inline(always)]
        pub fn decompress_tagged_fast(_on_heap_address: usize, _raw_value: Word) -> usize {
            unreachable!()
        }

        #[inline(always)]
        pub fn decompress_tagged(_raw_value: Word) -> usize {
            unreachable!()
        }
    }
}

pub use enabled::HeapCompressionScheme;