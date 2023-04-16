use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not};

pub const fn fast_bit_vector_array_length(num_bits: usize) -> usize {
    (num_bits + 31) / 32
}

pub struct FastBitVectorWordView<'a> {
    pub words: &'a [u32],
    pub num_bits: usize,
}

pub trait WordView {
    fn word(&self, index: usize) -> u32;
    fn num_bits(&self) -> usize;
}

impl<'a> WordView for FastBitVectorWordView<'a> {
    fn word(&self, index: usize) -> u32 {
        self.words[index]
    }

    fn num_bits(&self) -> usize {
        self.num_bits
    }
}

pub trait WordOwner {
    type View<'a>: WordView
    where
        Self: 'a;

    fn new() -> Self;

    fn view<'a>(&'a self) -> Self::View<'a>;

    fn word(&self, index: usize) -> u32 {
        self.view().word(index)
    }

    fn num_bits(&self) -> usize {
        self.view().num_bits()
    }

    fn word_mut(&mut self, index: usize) -> &mut u32;
    fn clear_all(&mut self);
    fn set_all(&mut self);
    fn resize(&mut self, num_bits: usize);
    fn array_length(&self) -> usize {
        fast_bit_vector_array_length(self.num_bits())
    }
}

#[derive(Clone)]
pub struct FastBitVectorWordOwner {
    pub words: Vec<u32>,
    pub num_bits: usize,
}

impl WordOwner for FastBitVectorWordOwner {
    type View<'a> = FastBitVectorWordView<'a>;

    fn view<'a>(&'a self) -> Self::View<'a> {
        FastBitVectorWordView {
            words: &self.words,
            num_bits: self.num_bits,
        }
    }

    fn word_mut(&mut self, index: usize) -> &mut u32 {
        &mut self.words[index]
    }

    fn clear_all(&mut self) {
        for word in self.words.iter_mut() {
            *word = 0;
        }
    }

    fn set_all(&mut self) {
        for word in self.words.iter_mut() {
            *word = !0;
        }
    }

    fn resize(&mut self, num_bits: usize) {
        if self.array_length() != fast_bit_vector_array_length(num_bits) {
            self.resize_slow(num_bits);
        }

        self.num_bits = num_bits;
    }

    fn new() -> Self {
        FastBitVectorWordOwner {
            words: Vec::new(),
            num_bits: 0,
        }
    }
}

impl FastBitVectorWordOwner {
    fn resize_slow(&mut self, num_bits: usize) {
        let new_length = fast_bit_vector_array_length(num_bits);

        self.words.resize(new_length, 0);
    }
}

pub struct FastBitVectorOrWords<Left: WordView, Right: WordView> {
    pub left: Left,
    pub right: Right,
}

impl<Left: WordView, Right: WordView> WordOwner for FastBitVectorOrWords<Left, Right> {
    type View<'a> = &'a FastBitVectorOrWords<Left, Right>
        where Left: 'a, Right: 'a;

    fn view<'a>(&'a self) -> Self::View<'a> {
        self
    }

    fn array_length(&self) -> usize {
        fast_bit_vector_array_length(self.left.num_bits())
    }

    fn clear_all(&mut self) {
        unreachable!()
    }

    fn new() -> Self {
        unreachable!()
    }

    fn num_bits(&self) -> usize {
        self.left.num_bits()
    }

    fn resize(&mut self, _num_bits: usize) {
        unreachable!()
    }

    fn set_all(&mut self) {
        unreachable!()
    }

    fn word(&self, index: usize) -> u32 {
        self.left.word(index) & self.right.word(index)
    }

    fn word_mut(&mut self, _index: usize) -> &mut u32 {
        unreachable!()
    }
}

impl<'a, T: WordOwner> WordView for &'a T {
    fn num_bits(&self) -> usize {
        <T as WordOwner>::View::num_bits(&self.view())
    }
    fn word(&self, index: usize) -> u32 {
        <T as WordOwner>::View::word(&self.view(), index)
    }
}

impl<Left: WordView, Right: WordView> WordOwner for FastBitVectorAndWords<Left, Right> {
    type View<'a> = &'a FastBitVectorAndWords<Left, Right>
        where Left: 'a, Right: 'a;

    fn view<'a>(&'a self) -> Self::View<'a> {
        self
    }

    fn array_length(&self) -> usize {
        fast_bit_vector_array_length(self.left.num_bits())
    }

    fn clear_all(&mut self) {
        unreachable!()
    }

    fn new() -> Self {
        unreachable!()
    }

    fn num_bits(&self) -> usize {
        self.left.num_bits()
    }

    fn resize(&mut self, _num_bits: usize) {
        unreachable!()
    }

    fn set_all(&mut self) {
        unreachable!()
    }

    fn word(&self, index: usize) -> u32 {
        self.left.word(index) & self.right.word(index)
    }

    fn word_mut(&mut self, _index: usize) -> &mut u32 {
        unreachable!()
    }
}

impl<Left: WordView, Right: WordView> WordView for FastBitVectorOrWords<Left, Right> {
    fn word(&self, index: usize) -> u32 {
        self.left.word(index) | self.right.word(index)
    }

    fn num_bits(&self) -> usize {
        self.left.num_bits()
    }
}

pub struct FastBitVectorNotWords<Left: WordView> {
    pub left: Left,
}

impl<Left: WordView> WordOwner for FastBitVectorNotWords<Left> {
    type View<'a> = &'a Self
    where Left: 'a;

    fn array_length(&self) -> usize {
        fast_bit_vector_array_length(self.left.num_bits())
    }

    fn clear_all(&mut self) {
        unreachable!()
    }

    fn new() -> Self {
        unreachable!()
    }

    fn num_bits(&self) -> usize {
        self.left.num_bits()
    }

    fn resize(&mut self, _num_bits: usize) {
        unreachable!()
    }

    fn set_all(&mut self) {
        unreachable!()
    }

    fn view<'a>(&'a self) -> Self::View<'a> {
        self
    }

    fn word(&self, index: usize) -> u32 {
        !self.left.word(index)
    }

    fn word_mut(&mut self, _index: usize) -> &mut u32 {
        unreachable!()
    }
}
pub struct FastBitVectorAndWords<Left: WordView, Right: WordView> {
    pub left: Left,
    pub right: Right,
}

pub struct FastBitVectorImpl<Words: WordOwner> {
    pub words: Words,
}

impl<Words: WordOwner> FastBitVectorImpl<Words> {
    pub fn view(&self) -> Words::View<'_> {
        self.words.view()
    }

    pub fn new() -> Self {
        FastBitVectorImpl {
            words: Words::new(),
        }
    }

    pub fn num_bits(&self) -> usize {
        self.words.num_bits()
    }

    pub fn len(&self) -> usize {
        self.num_bits()
    }

    pub fn array_length(&self) -> usize {
        self.words.array_length()
    }

    fn at_impl(&self, index: usize) -> bool {
        let word = self.words.word(index >> 5);
        let bit = 1 << (index & 31);

        (word & bit) != 0
    }

    pub fn at(&self, index: usize) -> bool {
        if index >= self.num_bits() {
            false
        } else {
            self.at_impl(index)
        }
    }

    pub fn bit_count(&self) -> usize {
        let mut count = 0;

        let mut ix = self.array_length();

        while ix > 0 {
            ix -= 1;
            let word = self.words.word(ix);
            count += word.count_ones() as usize;
        }

        count
    }

    pub fn for_each_set_bit(&self, mut f: impl FnMut(usize)) {
        for i in 0..self.array_length() {
            let mut word = self.words.word(i);

            let mut j = i * 32;

            while word != 0 {
                if word & 1 != 0 {
                    f(j);
                }

                word >>= 1;
                j += 1;
            }
        }
    }

    pub fn at_mut(&mut self, index: usize) -> FastBitReference {
        let word = self.words.word_mut(index >> 5);
        let bit = 1 << (index & 31);

        FastBitReference::new(word, bit)
    }

    pub fn for_each_clear_bit(&self, mut f: impl FnMut(usize)) {
        self.not().for_each_set_bit(|i| f(i));
    }

    pub fn is_empty(&self) -> bool {
        self.bit_count() == 0
    }
}

impl<Words: WordOwner> PartialEq for FastBitVectorImpl<Words> {
    fn eq(&self, other: &Self) -> bool {
        if self.num_bits() != other.num_bits() {
            return false;
        }

        let mut ix = self.array_length();

        while ix > 0 {
            if self.words.word(ix) != other.words.word(ix) {
                return false;
            }

            ix -= 1;
        }

        true
    }
}

impl<Words: WordOwner> Eq for FastBitVectorImpl<Words> {}

impl<Words: WordOwner> WordView for FastBitVectorImpl<Words> {
    fn word(&self, index: usize) -> u32 {
        self.words.word(index)
    }

    fn num_bits(&self) -> usize {
        self.words.num_bits()
    }
}

impl<'a, Words: WordOwner, OtherWords: WordView> BitAnd<OtherWords>
    for &'a FastBitVectorImpl<Words>
{
    type Output = FastBitVectorImpl<FastBitVectorAndWords<Words::View<'a>, OtherWords>>;
    fn bitand(self, rhs: OtherWords) -> Self::Output {
        FastBitVectorImpl {
            words: FastBitVectorAndWords {
                left: self.words.view(),
                right: rhs,
            },
        }
    }
}

impl<'a, Words: WordOwner, OtherWords: WordView> BitOr<OtherWords>
    for &'a FastBitVectorImpl<Words>
{
    type Output = FastBitVectorImpl<FastBitVectorOrWords<Words::View<'a>, OtherWords>>;
    fn bitor(self, rhs: OtherWords) -> Self::Output {
        FastBitVectorImpl {
            words: FastBitVectorOrWords {
                left: self.words.view(),
                right: rhs,
            },
        }
    }
}

impl<'a, Words: WordOwner> Not for &'a FastBitVectorImpl<Words> {
    type Output = FastBitVectorImpl<FastBitVectorNotWords<Words::View<'a>>>;

    fn not(self) -> Self::Output {
        FastBitVectorImpl {
            words: FastBitVectorNotWords {
                left: self.words.view(),
            },
        }
    }
}

pub struct FastBitReference<'a> {
    word: &'a mut u32,
    mask: u32,
}

impl<'a> FastBitReference<'a> {
    pub fn new(word: &'a mut u32, mask: u32) -> Self {
        FastBitReference { word, mask }
    }

    pub fn get(&self) -> bool {
        (*self.word & self.mask) != 0
    }

    pub fn set(&mut self, value: bool) {
        if value {
            *self.word |= self.mask;
        } else {
            *self.word &= !self.mask;
        }
    }
}

pub type FastBitVector = FastBitVectorImpl<FastBitVectorWordOwner>;

impl FastBitVector {
    pub fn with_capacity(num_bits: usize) -> Self {
        let mut this = FastBitVector::new();
        this.grow(num_bits);
        this
    }

    pub fn with_value(num_bits: usize, value: bool) -> Self {
        let mut this = FastBitVector::with_capacity(num_bits);
        this.fill(value);
        this
    }

    pub fn grow(&mut self, num_bits: usize) {
        self.words.resize(num_bits);
    }

    pub fn fill(&mut self, value: bool) {
        if !value {
            self.words.clear_all();
        } else {
            self.words.set_all();
        }
    }

    pub fn set_all(&mut self) {
        self.words.set_all();
    }

    pub fn clear_all(&mut self) {
        self.words.clear_all();
    }
}

impl<OtherWords: WordView> BitAndAssign<OtherWords> for FastBitVector {
    fn bitand_assign(&mut self, rhs: OtherWords) {
        let mut ix = self.array_length();

        while ix > 0 {
            ix -= 1;
            let word = self.words.word_mut(ix);
            *word &= rhs.word(ix);
        }
    }
}

impl<OtherWords: WordView> BitOrAssign<OtherWords> for FastBitVector {
    fn bitor_assign(&mut self, rhs: OtherWords) {
        let mut ix = self.array_length();

        while ix > 0 {
            ix -= 1;
            let word = self.words.word_mut(ix);
            *word |= rhs.word(ix);
        }
    }
}
