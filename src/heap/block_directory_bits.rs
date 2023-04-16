#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BlockDirectoryBitsKind {
    Live,
    Empty,
    Allocated,
    CanAllocateButNotEmpty,
    Destructible,
    Eden,
    Unswept,
    MarkingNotEmpty,
}

use crate::utils::fast_bitvec::*;

const BITS_PER_SEGMENT: usize = 32;
const SEGMENT_SHIFT: usize = 5;
const INDEX_MASK: usize = (1 << SEGMENT_SHIFT) - 1;

const NUMBER_OF_BLOCK_DIRECTORY_BITS: usize = 8;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub struct Segment {
    pub data: [u32; NUMBER_OF_BLOCK_DIRECTORY_BITS],
}

pub struct BlockDirectoryBitsWordView<'a, const KIND: BlockDirectoryBitsKind> {
    segments: &'a [Segment],
    num_bits: usize,
}

impl<'a, const KIND: BlockDirectoryBitsKind> WordView for BlockDirectoryBitsWordView<'a, KIND> {
    fn num_bits(&self) -> usize {
        self.num_bits
    }

    fn word(&self, index: usize) -> u32 {
        self.segments[index].data[KIND as usize]
    }
}

impl<'a, const KIND: BlockDirectoryBitsKind> WordOwner for BlockDirectoryBitsWordView<'a, KIND> {
    fn word_mut(&mut self, _index: usize) -> &mut u32 {
        unreachable!()
    }

    fn array_length(&self) -> usize {
        fast_bit_vector_array_length(self.num_bits)
    }

    fn clear_all(&mut self) {
        unreachable!()
    }

    fn new() -> Self {
        unreachable!()
    }

    fn num_bits(&self) -> usize {
        self.num_bits
    }
    fn resize(&mut self, _num_bits: usize) {
        unreachable!()
    }

    fn set_all(&mut self) {
        unreachable!()
    }

    fn view<'b>(&'b self) -> Self::View<'b> {
        BlockDirectoryBitsWordView {
            segments: self.segments,
            num_bits: self.num_bits,
        }
    }

    fn word(&self, index: usize) -> u32 {
        self.segments[index].data[KIND as usize]
    }

    type View<'b> = BlockDirectoryBitsWordView<'b, KIND>
        where Self: 'b;
}

pub struct BlockDirectoryBitsWordOwner<'a, const KIND: BlockDirectoryBitsKind> {
    segments: &'a mut [Segment],
    num_bits: usize,
}

impl<'a, const KIND: BlockDirectoryBitsKind> WordOwner for BlockDirectoryBitsWordOwner<'a, KIND> {
    type View<'b> = BlockDirectoryBitsWordView<'b, KIND>
        where Self: 'b;
    fn clear_all(&mut self) {
        for index in 0..fast_bit_vector_array_length(self.num_bits) {
            self.segments[index].data[KIND as usize] = 0;
        }
    }

    fn word(&self, index: usize) -> u32 {
        self.segments[index].data[KIND as usize]
    }

    fn word_mut(&mut self, index: usize) -> &mut u32 {
        &mut self.segments[index].data[KIND as usize]
    }

    fn array_length(&self) -> usize {
        fast_bit_vector_array_length(self.num_bits)
    }

    fn new() -> Self {
        todo!()
    }

    fn num_bits(&self) -> usize {
        self.num_bits
    }

    fn resize(&mut self, _num_bits: usize) {
        todo!()
    }

    fn set_all(&mut self) {
        todo!()
    }

    fn view<'b>(&'b self) -> Self::View<'b> {
        BlockDirectoryBitsWordView {
            segments: self.segments,
            num_bits: self.num_bits,
        }
    }
}

pub type BlockDirectoryBitsView<'a, const KIND: BlockDirectoryBitsKind> =
    FastBitVectorImpl<BlockDirectoryBitsWordView<'a, KIND>>;

pub type BlockDirectoryBitsRef<'a, const KIND: BlockDirectoryBitsKind> =
    FastBitVectorImpl<BlockDirectoryBitsWordOwner<'a, KIND>>;

pub struct BlockDirectoryBits {
    segments: Vec<Segment>,
    num_bits: usize,
}

impl BlockDirectoryBits {
    pub fn live(&self) -> BlockDirectoryBitsView<'_, { BlockDirectoryBitsKind::Live }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordView {
                segments: &self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn empty(&self) -> BlockDirectoryBitsView<'_, { BlockDirectoryBitsKind::Empty }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordView {
                segments: &self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn allocated(&self) -> BlockDirectoryBitsView<'_, { BlockDirectoryBitsKind::Allocated }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordView {
                segments: &self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn can_allocate_but_not_empty(
        &self,
    ) -> BlockDirectoryBitsView<'_, { BlockDirectoryBitsKind::CanAllocateButNotEmpty }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordView {
                segments: &self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn destructible(
        &self,
    ) -> BlockDirectoryBitsView<'_, { BlockDirectoryBitsKind::Destructible }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordView {
                segments: &self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn eden(&self) -> BlockDirectoryBitsView<'_, { BlockDirectoryBitsKind::Eden }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordView {
                segments: &self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn unswept(&self) -> BlockDirectoryBitsView<'_, { BlockDirectoryBitsKind::Unswept }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordView {
                segments: &self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn marking_not_empty(
        &self,
    ) -> BlockDirectoryBitsView<'_, { BlockDirectoryBitsKind::MarkingNotEmpty }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordView {
                segments: &self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn live_mut(&mut self) -> BlockDirectoryBitsRef<'_, { BlockDirectoryBitsKind::Live }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordOwner {
                segments: &mut self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn empty_mut(&mut self) -> BlockDirectoryBitsRef<'_, { BlockDirectoryBitsKind::Empty }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordOwner {
                segments: &mut self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn allocated_mut(
        &mut self,
    ) -> BlockDirectoryBitsRef<'_, { BlockDirectoryBitsKind::Allocated }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordOwner {
                segments: &mut self.segments,
                num_bits: self.num_bits,
            },
        }
    }


    pub fn can_allocate_but_not_empty_mut(
        &mut self,
    ) -> BlockDirectoryBitsRef<'_, { BlockDirectoryBitsKind::CanAllocateButNotEmpty }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordOwner {
                segments: &mut self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn destructible_mut(
        &mut self,
    ) -> BlockDirectoryBitsRef<'_, { BlockDirectoryBitsKind::Destructible }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordOwner {
                segments: &mut self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn eden_mut(&mut self) -> BlockDirectoryBitsRef<'_, { BlockDirectoryBitsKind::Eden }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordOwner {
                segments: &mut self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn unswept_mut(&mut self) -> BlockDirectoryBitsRef<'_, { BlockDirectoryBitsKind::Unswept }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordOwner {
                segments: &mut self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn marking_not_empty_mut(
        &mut self,
    ) -> BlockDirectoryBitsRef<'_, { BlockDirectoryBitsKind::MarkingNotEmpty }> {
        FastBitVectorImpl {
            words: BlockDirectoryBitsWordOwner {
                segments: &mut self.segments,
                num_bits: self.num_bits,
            },
        }
    }

    pub fn is_live(&self, index: usize) -> bool {
        self.live().at(index)
    }

    pub fn is_empty(&self, index: usize) -> bool {
        self.empty().at(index)
    }

    pub fn is_allocated(&self, index: usize) -> bool {
        self.allocated().at(index)
    }

    pub fn is_can_allocate_but_not_empty(&self, index: usize) -> bool {
        self.can_allocate_but_not_empty().at(index)
    }

    pub fn is_destructible(&self, index: usize) -> bool {
        self.destructible().at(index)
    }

    pub fn is_eden(&self, index: usize) -> bool {
        self.eden().at(index)
    }

    pub fn is_unswept(&self, index: usize) -> bool {
        self.unswept().at(index)
    }

    pub fn is_marking_not_empty(&self, index: usize) -> bool {
        self.marking_not_empty().at(index)
    }

    pub fn set_live(&mut self, index: usize) {
        self.live_mut().at_mut(index).set(true);
    }

    pub fn set_empty(&mut self, index: usize) {
        self.empty_mut().at_mut(index).set(true);
    }

    pub fn set_allocated(&mut self, index: usize) {
        self.allocated_mut().at_mut(index).set(true);
    }

    pub fn set_can_allocate_but_not_empty(&mut self, index: usize) {
        self.can_allocate_but_not_empty_mut().at_mut(index).set(true);
    }

    pub fn set_destructible(&mut self, index: usize) {
        self.destructible_mut().at_mut(index).set(true);
    }

    pub fn set_eden(&mut self, index: usize) {
        self.eden_mut().at_mut(index).set(true);
    }

    pub fn set_unswept(&mut self, index: usize) {
        self.unswept_mut().at_mut(index).set(true);
    }

    pub fn set_marking_not_empty(&mut self, index: usize) {
        self.marking_not_empty_mut().at_mut(index).set(true);
    }

    pub fn for_each_segment(&self, mut functor: impl FnMut(usize, &Segment)) {
        for (index, segment) in self.segments.iter().enumerate() {
            functor(index, segment);
        }
    }

    pub fn for_each_segment_mut(&mut self, mut functor: impl FnMut(usize, &mut Segment)) {
        for (index, segment) in self.segments.iter_mut().enumerate() {
            functor(index, segment);
        }
    }

    pub fn resize(&mut self, num_bits: usize) {
        let old_num_bits = self.num_bits;
        self.segments.resize(
            fast_bit_vector_array_length(num_bits),
            Segment {
                data: [0; NUMBER_OF_BLOCK_DIRECTORY_BITS],
            },
        );
        self.num_bits = num_bits;
        let used_bits_in_last_segment = num_bits & INDEX_MASK;

        if num_bits < old_num_bits && used_bits_in_last_segment != 0 {
            assert!(used_bits_in_last_segment < BITS_PER_SEGMENT);
            let segment = self.segments.last_mut().unwrap();
            let mask = (1 << used_bits_in_last_segment) - 1;

            for index in 0..NUMBER_OF_BLOCK_DIRECTORY_BITS {
                segment.data[index] &= mask;
            }
        }
    }

    pub fn new(num_bits: usize) -> Self {
        let mut segments = Vec::new();
        segments.resize(fast_bit_vector_array_length(num_bits), Segment::default());
        Self {
            segments,
            num_bits,
        }
    }
}
