

(define dsl-run #f)


(let ()
    (define (string-join lst sep)
        (apply string-append (map* (lambda (x) (string-append x sep)) lst)))
    (define (map* f lst)
        (let loop ([lst lst] [res '()])
            (if (null? lst)
                (reverse res)
                (loop (cdr lst) (cons (f (car lst)) res)))))
    
    (define (string-replace str from-char to-char)  
        (list->string (let loop ([str (string->list str)] [res '()])
            (if (null? str)
                (reverse res)
                (loop (cdr str) (cons (if (char=? (car str) from-char) to-char (car str)) res))))))

    ; (define (substring string start end) ...)
    (define (string-ends-with s1 s2)
        (let ([len1 (string-length s1)] [len2 (string-length s2)])
            (and (>= len1 len2)
                (string=? (substring s1 (- len1 len2) len1) s2))))
    (define-record-type (argument %make-argument argument?)
        (fields 
            (mutable name)
            (mutable index)
            (mutable type)
            (immutable optional)))

    (define (make-argument name type index)
        (let ([optional (string-ends-with (symbol->string name) "?")])
            (%make-argument name index type optional)))
    
    (define (argument-field arg)
        (format "pub ~a: ~a," (argument-name arg) (argument-type arg)))
    (define (argument-create-param arg)
        (format "~a: ~a," (argument-name arg) (argument-type arg)))
    (define (argument-create-ref-param arg)
        (format "~a: &~a," (argument-name arg) (argument-type arg)))
    
    (define (argument-create-mut-param arg)
        (format "~a: &mut ~a," (argument-name arg) (argument-type arg)))

    (define (argument-fits-check arg size)
        (fits-check size (argument-name arg) (argument-type arg)))
    (define (argument-fits-write arg size)
        (fits-write size (argument-name arg) (argument-type arg)))
    
    (define (argument-setter arg)
        (let ([name (argument-name arg)] [type (argument-type arg)] [index (argument-index arg)])
        (format 
        "
        pub fn set_~a<F>(&mut self, value: ~a, f: F)
        where F: FnOnce() -> ~a {
            if self.is_wide32() { self.set_~a_sized::<{OpcodeSize::Wide32}, F>(value, f) }
            else if self.is_wide16() { self.set_~a_sized::<{OpcodeSize::Wide16}, F>(value, f) }
            else { self.set_~a_sized::<{OpcodeSize::Narrow}, F>(value, f) }
        }
        pub fn set_~a_sized<const SIZE: OpcodeSize, F>(&mut self, mut value: ~a, f: F) 
        where F: FnOnce() -> ~a {
            if !~a {
                value = f();
            }
            unsafe {
                match SIZE {
                    OpcodeSize::Narrow => {
                        let stream = (self as *mut Self as *mut u8)
                            .add(
                                ~a * SIZE as usize 
                                + PaddingBySize::<{OpcodeSize::Narrow}>::VALUE
                                + OpcodeIDWidthBySize::<{OpcodeSize::Narrow}>::VALUE) as *mut <TypeBySize::<{OpcodeSize::Narrow}> as TypeBySizeTrait>::UnsignedType;
                        stream.write(~a as _);
                    }

                    OpcodeSize::Wide16 => {
                        let stream = (self as *mut Self as *mut u8)
                            .add(
                                ~a * SIZE as usize 
                                + PaddingBySize::<{OpcodeSize::Wide16}>::VALUE
                                + OpcodeIDWidthBySize::<{OpcodeSize::Wide16}>::VALUE) as *mut <TypeBySize::<{OpcodeSize::Wide16}> as TypeBySizeTrait>::UnsignedType;
                        stream.write(~a as _);
                    }

                    OpcodeSize::Wide32 => {
                        let stream = (self as *mut Self as *mut u8)
                            .add(
                                ~a * SIZE as usize 
                                + PaddingBySize::<{OpcodeSize::Wide32}>::VALUE
                                + OpcodeIDWidthBySize::<{OpcodeSize::Wide32}>::VALUE) as *mut <TypeBySize::<{OpcodeSize::Wide32}> as TypeBySizeTrait>::UnsignedType;
                        stream.write(~a as _);
                    }
                }
            }
        }
            
            
        " 
        ; set_~a 
        name type type name name name
        ; set_~a_sized 
        name type type (fits-check "SIZE" "value" type)
        index (fits-convert "OpcodeSize::Narrow" "value" type)
        index (fits-convert "OpcodeSize::Wide16" "value" type)
        index (fits-convert "OpcodeSize::Wide32" "value" type)
         )))

    (define (argument-load-from-stream arg index size)
        (format "~a: ~a" (argument-name arg) (fits-convert-back size (format "stream.add(~a).read_unaligned()" index) (argument-type arg))))
    (define (fits-convert size name type)
        (cond 
            [(string=? size "SIZE")
                (format 
"match SIZE {
    OpcodeSize::Narrow => Fits::<~a, {OpcodeSize::Narrow}>::convert(~a),
    OpcodeSize::Wide16 => Fits::<~a, {OpcodeSize::Wide16}>::convert(~a),
    OpcodeSize::Wide32 => Fits::<~a, {OpcodeSize::Wide32}>::convert(~a)
}
"
                type name type name type name)
            
            ]
            [else 
                (format "Fits::<~a, {~a}>::convert(~a)" type size name)]))
    (define (fits-convert-back size name type)
        (format "Fits::<~a, {~a}>::convert_back(~a as _)" type size name))
    (define (fits-check size name type)
        (cond 
            [(string=? size "SIZE")
                (format 
"(match SIZE {
    OpcodeSize::Narrow => Fits::<~a, {OpcodeSize::Narrow}>::check(~a),
    OpcodeSize::Wide16 => Fits::<~a, {OpcodeSize::Wide16}>::check(~a),
    OpcodeSize::Wide32 => Fits::<~a, {OpcodeSize::Wide32}>::check(~a)
})
"
                type name type name type name)
            ]
            [else 
                (format "Fits::<~a, {~a}>::check(~a)" type size name)]))
    (define (fits-write size name type)
                (cond 
            [(string=? size "SIZE")
                (format 
"match SIZE {
    OpcodeSize::Narrow => gen.write(Fits::<~a, {OpcodeSize::Narrow}>::convert(~a)),
    OpcodeSize::Wide16 => gen.write(Fits::<~a, {OpcodeSize::Wide16}>::convert(~a)),
    OpcodeSize::Wide32 => gen.write(Fits::<~a, {OpcodeSize::Wide32}>::convert(~a))
}
"
                type name type name type name)
            
            ]
            [else 
                (format "gen.write(Fits::<~a, {~a}>::convert(~a));" type size name)]))
        
        ;(format "gen.write(~a);" (fits-convert size name type)))

    
    (define-record-type (opcode-group make-opcode-group opcode-group?)
        (fields 
            (immutable section)
            (immutable name)
            (mutable opcodes)
            (mutable config)))
    
    (define-record-type (section %make-section section?)
        (fields 
            (immutable name)
            (mutable config)
            (mutable opcodes section-opcodes set-section-opcodes!)
            (mutable opcode-groups section-opcode-groups set-section-opcode-groups!)))
    (define (make-section name config)
        (%make-section name config '() '()))

    (define (type-to-*const type)
        (string->symbol (format "*const ~a" type)))
    (define (type-to-*mut type)
        (string->symbol (format "*mut ~a" type)))

    (define (type-to-ref type)
        (string->symbol (format "&~a" type)))
    
    (define (type-to-mut type)
        (string->symbol (format "&mut ~a" type)))

    (define-record-type (opcode %make-opcode opcode?)
        (fields 
            (mutable section)
            (mutable name)
            (mutable extras)
            (mutable metadata)
            (mutable args)
            (mutable tmps)
            (immutable docs)
            (mutable id opcode-id set-opcode-id!)))
    
    (define create-id 
        (let ([id 0])
            (lambda ()
                (set! id (+ id 1))
                id)))

    (define (opcode-create-id! opcode)
        (set-opcode-id! opcode (create-id)))

    (define (map*-with-index f lst)
        (let ([id 0])
            (map* (lambda (x)
                (let ([res (f x id)])
                    (set! id (+ id 1))
                    res))
                lst)))
    (define (string-split char-delimiter? string)
        (define (maybe-add a b parts)
            (if (= a b) parts (cons (substring string a b) parts)))
        (let ((n (string-length string)))
            (let loop ((a 0) (b 0) (parts '()))
            (if (< b n)
                (if (not (char-delimiter? (string-ref string b)))
                    (loop a (+ b 1) parts)
                    (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
                (reverse (maybe-add a b parts))))))
    (define (char-underscore? ch) (char=? ch #\_))
    (define (char-minus? ch) (char=? ch #\-))
    (define (make-opcode section name extras args metadata metadata-inits tmps docs)
        (%make-opcode 
            section
            name 
            extras
            (make-metadata (if (not metadata) '() metadata) metadata-inits)
            (map*-with-index 
                (lambda (arg index)
                    (make-argument (car arg) (cadr arg) index))
                args)
            tmps
            docs
            #f))
    (define (opcode-capitalized-name op)
        (apply string-append "Op" (map* 
            (lambda (s)
                (string-titlecase s))
            (string-split char-minus? (symbol->string (opcode-name op))))))
    (define (opcode-upcase-name op)
        ; replace - with _ and do string-upcase
        (string-append "OP_" (string-replace (string-upcase (symbol->string (opcode-name op))) #\- #\_)))

    (define-record-type (metadata make-metadata metadata?)
        (fields 
            (mutable fields)
            (mutable initializers)))
    
    (define (metadata-empty? metadata)
        (null? (metadata-fields metadata)))
    (define (metadata-length metadata)
        (length (metadata-fields metadata)))
    
    (define (metadata-load-from-stream metadata index size)
        (if (metadata-empty? metadata)
            ""
            (format "~a: ~a" metadata-field-name (fits-convert size (format "stream.add(~a).read_unaligned()" index) 'usize))))

    (define (metadata-struct metadata op)
        (cond 
            [(metadata-empty? metadata) ""]
            [else 
                (let ()
                (define (generate-offset-of-functions prefix field-names)
                    (apply string-append (map* 
                        (lambda (field-name)
                            (format "~astatic OFFSET_OF_~a: usize = offset_of!(Metadata, ~a);~%"
                                prefix (string-upcase field-name) field-name))
                        field-names)))
                (define (convert-fields prefix fields)
                    (apply string-append (map* 
                        (lambda (field type)
                            (cond 
                                [(pair? type)
                                    (error 'convert-fields "NYI")]
                                [else
                                    (set! field-names (cons (symbol->string field) field-names))
                                    (format "~apub ~a: ~a," prefix field type)]))
                        fields)))
                (define prefix "        ")
                (define field-names '())
                (define fields (convert-fields prefix (metadata-fields metadata)))
                (let (
                    [fields (string-append fields "\n" generate-offset-of-functions prefix field-names)]
                    [inits 
                        (cond 
                            [(null? (metadata-initializers metadata)) '()]
                            [else 
                                (map* 
                                    (lambda (init)
                                        (format "~a: _op.~a,~%" (car init) (cadr init)))
                                    (metadata-initializers metadata))])])
                    (format
"pub struct Metadata {
    ~a
}
impl Metadata {
    pub fn new(_op: &~a) -> Self {
        Self {
            ~a
        }
    }
}
"
                        fields 
                        (opcode-capitalized-name op)
                        inits)))]))

    (define (metadata-field meta prefix)
        (if (metadata-empty? meta)
            ""
            (format "~%~apub ~a: usize," prefix metadata-field-name)))

    (define metadata-field-name 'metadata_id)

    (define metadata-emitter-local
        (let ([emitter-local #f])
            (lambda (meta)
                (unless emitter-local
                    (set! emitter-local (make-argument '_metadata_id 'usize -1)))
                emitter-local )))
    (define (metadata-create-emitter-local meta)
        (if (metadata-empty? meta)
            ""
            (format "let mut _metadata_id = gen.add_metadata_for(opcode_id);~%")))
    

    (define (assq* key lst)
        (let ([res (assq key lst)])
            (if res (cdr res) #f)))
    
    (define (identity x) x)


    (define (parse-opcode-name name)
        ; parse `?`, `=?`, `<?` and others into proper names
        ; 'op?' - 'is-op'
        ; 'op=?' - 'is-op-eq'
        ; 'op<?' - 'is-op-lt'
        ; etc

        (cond 
            [(string=? name "=?") "is-numerically-equal"]
            [(string=? name ">?") "is-numerically-less"]
            [(string=? name "<?") "is-numerically-greater"]
            [(string=? name ">=?") "is-numerically-greatereq"]
            [(string=? name "<=?") "is-numerically-lesseq"]
            [(string-ends-with name ">=?") (string-append "is-" (substring name 0 (- (string-length name) 3)) "-greatereq")]
            [(string-ends-with name "<=?") (string-append "is-" (substring name 0 (- (string-length name) 3)) "-lesseq")]
            [(string-ends-with name ">?") (string-append "is-" (substring name 0 (- (string-length name) 2)) "-greater")]
            [(string-ends-with name "<?") (string-append "is-" (substring name 0 (- (string-length name) 2)) "-less")]
            [(string-ends-with name "=?") (string-append "is-" (substring name 0 (- (string-length name) 2)) "-equal")]
            [(string-ends-with name "?") (string-append "is-" (substring name 0 (- (string-length name) 1)))]
            [else name]
        )
    
    )

    (define (section-create-opcode  section name config)
        (make-opcode
            section 
            (string->symbol (parse-opcode-name (symbol->string name))) 
            (cond ((assq* 'extras config) => identity) (else '()))
            (cond ((assq* 'args config) => identity) (else '()))
            (cond ((assq* 'metadata config) => identity) (else '()))
            (cond ((assq* 'metadata-inits config) => identity) (else '()))
            (cond ((assq* 'tmps config) => identity) (else '()))
            (cond ((assq* 'docs config) => car) (else #f))))

    (define (section-add-opcode section name config)
        (set-section-opcodes! section
            (cons (section-create-opcode section name config)
                (section-opcodes section))))

    (define (section-add-opcode-group section name ops config)
        (let ([opcodes (map* 
                         (lambda (opcode) (section-create-opcode section opcode config))
                         ops)])
            (set-section-opcode-groups! section
                (cons (make-opcode-group section name opcodes config)
                    (section-opcode-groups section)))
            (set-section-opcodes! section
                (append opcodes (section-opcodes section)))))
    (define (section-create-ids! section)
        (for-each 
            (lambda (op)
                (opcode-create-id! op))
            (section-opcodes section)))
    
    (define (section-helpers section num-opcodes)
        (let ([config (section-config section)])
            (call-with-string-output-port 
                (lambda (out)
                    (when (assq* 'emit-in-macro-file config)
                        (display (format "#[macro_export] macro_rules! for_each_~a_id {~%($m: path) => {~%$m!{" (string-downcase (assq* 'macro-name-component config))) out)
                        (for-each 
                            (lambda (op)
                                (display (format "(~a, ~a)~%" (opcode-upcase-name op) (opcode-length op)) out))
                            (section-opcodes section))
                        (display "}\n};}\n" out)
                        (display (format "#[macro_export] macro_rules! for_each_~a_name {~%($m: path) => {~%$m!{" (string-downcase (assq* 'macro-name-component config))) out)
                        (for-each 
                            (lambda (op)
                                (display (format "\"~a\"" (opcode-name op)) out))
                            (section-opcodes section))
                        (display "}\n};}\n" out)
                        (display (format "pub const NUMBER_OF_~a_IDS: usize = ~a;" (string-upcase (assq* 'macro-name-component config)) (length (section-opcodes section))) out))))))

    (define (opcode-opcode-id op)
        (format "pub const OPCODE_ID: OpcodeID = ~a;" (opcode-upcase-name op)))
    (define (opcode-length op)
        (+ (length (opcode-args op)) (if (metadata-empty? (opcode-metadata op)) 0 1)))

    (define (opcode-length-value op)
        (format "pub const OPCODE_LENGTH: usize = ~a;" (opcode-length op)))
    
    (define (opcode-documentation op)
        (let ([docs (opcode-docs op)])
            (cond 
                [docs 
                    (apply string-append (map* 
                        (lambda (str)
                            (format "/// ~a~%" str))
                        (string-split (lambda (x) (char=? x #\linefeed)) docs)))]
                [else ""])))

    (define (opcode-emitter-impl op)
        (let (
            [op-wide16 (make-argument 'OP_WIDE16 'OpcodeID 0)]
            [op-wide32 (make-argument 'OP_WIDE32 'OpcodeID 0)]
            [metadata-param
                (if (metadata-empty? (opcode-metadata op))
                    ""
                    (format ", ~a" (argument-create-param (metadata-emitter-local (opcode-metadata op)))))]
            [metadata-arg 
                (if (metadata-empty? (opcode-metadata op))
                    ""
                    ", _metadata_id")])
            (format 
"
fn emit_impl<const SIZE: OpcodeSize>(gen: &mut BytecodeGenerator, ~a~a) -> bool {
if SIZE == OpcodeSize::Wide16 {
    gen.align_wide_opcode16();
} else if SIZE == OpcodeSize::Wide32 {
    gen.align_wide_opcode32();
}

if Self::check_impl::<{SIZE}>(gen,~a~a) {
    gen.record_opcode(Self::OPCODE_ID);
    if SIZE == OpcodeSize::Wide16 {
        ~a
    } else if SIZE == OpcodeSize::Wide32 {
        ~a
    }
    ~a
    ~a
    return true;
}

false 
}
"
(opcode-typed-args op)
metadata-param 
(opcode-untyped-args op)
metadata-arg 
(argument-fits-write op-wide16 "OpcodeSize::Narrow")
(argument-fits-write op-wide32 "OpcodeSize::Narrow")
(argument-fits-write (make-argument 'Self::OPCODE_ID 'OpcodeID 0) "OpcodeSize::Narrow")
(apply string-append (opcode-map*-operands-with-size op "        " "SIZE" (lambda (arg size) (argument-fits-write arg size))))
)))

    (define (opcode-dumper op)
        (format 
"pub fn dump(&self, dumper: &mut BytecodeDumper, _location: usize, _size_shift_amount: usize) {
dumper.print_location_and_op(_location, &\"**~a\"[2 - _size_shift_amount..]);
~a
}
"
        (opcode-name op)
        (opcode-print-args 
            op 
            (lambda (arg)
                (format "dumper.dump_operand(\"~a\", self.~a, ~a);" 
                    (argument-name arg)
                    (argument-name arg)
                    (if (zero? (argument-index arg)) "true" "false"))))))

    (define (dump-bytecode opcodes)
        (format 
"pub unsafe fn raw_dump_bytecode(dumper: &mut BytecodeDumper, _location: usize, instruction: *const BaseInstruction) {
    match (*instruction).opcode_id() {
~a
x => unreachable!(\"unknown opcode: {}\", x)
    }   
}"
(string-join (map 
    (lambda (op)
        (format 
"~a => (~a::decode(instruction as _)).dump(dumper, _location, (*instruction).size_shift_amount()),"
(opcode-upcase-name op) (opcode-capitalized-name op)
))
    opcodes)        
    "\n")))
    (define (opcode-emitter op)
        (let (
            [typed-args (opcode-typed-args op)]
            [untyped-args (opcode-untyped-args op)]
            [metadata-param
                (if (metadata-empty? (opcode-metadata op))
                    ""
                    (format ", ~a" (argument-create-param (metadata-emitter-local (opcode-metadata op)))))]
            [metadata-arg 
                (if (metadata-empty? (opcode-metadata op))
                    ""
                    ", _metadata_id")]
            [emitter-local (metadata-create-emitter-local (opcode-metadata op))]
        )
        (format 
"
pub fn emit_generic<const SIZE: OpcodeSize>(gen: &mut BytecodeGenerator, ~a) -> bool {
    Self::emit_impl::<SIZE>(gen, ~a)
}

pub fn emit(gen: &mut BytecodeGenerator, ~a) {
    Self::emit_with_smallest_size_requirement::<{OpcodeSize::Narrow}>(gen, ~a);
}

pub fn emit_narrow(gen: &mut BytecodeGenerator, ~a) -> bool {
    Self::emit_impl::<{OpcodeSize::Narrow}>(gen, ~a)
}

pub fn emit_wide16(gen: &mut BytecodeGenerator, ~a) -> bool {
    Self::emit_impl::<{OpcodeSize::Wide16}>(gen, ~a)
}

pub fn emit_wide32(gen: &mut BytecodeGenerator, ~a) -> bool {
    Self::emit_impl::<{OpcodeSize::Wide32}>(gen, ~a)
}

pub fn emit_with_smallest_size_requirement<const SIZE: OpcodeSize>(gen: &mut BytecodeGenerator, ~a) {
    ~a
    if SIZE as usize <= OpcodeSize::Narrow as usize {
        if Self::emit_narrow(gen,~a~a) {
            return;
        }
    }

    if SIZE as usize <= OpcodeSize::Wide16 as usize {
        if Self::emit_wide16(gen, ~a~a) {
            return;
        }
    }

    Self::emit_wide32(gen, ~a~a);
}
"
typed-args
untyped-args
typed-args
untyped-args
typed-args
untyped-args
typed-args
untyped-args
typed-args
untyped-args
typed-args
emitter-local
untyped-args metadata-arg 
untyped-args metadata-arg
untyped-args metadata-arg
)
        ))

    (define (opcode-check-impl op)
        (format 
"
fn check_impl<const SIZE: OpcodeSize>(gen: &mut BytecodeGenerator, ~a) -> bool {
let _ = gen;
true~a    
}
"   
        (opcode-typed-args op)
        (apply string-append (opcode-map*-fields-with-size op "" "SIZE"
            (lambda (arg size)
                (string-append "\n && "(argument-fits-check arg size)))))))

    (define (opcode-struct op)
        (let (
            [id (opcode-opcode-id op)]
            [length-value (opcode-length-value op)]
            [capitalized-name (opcode-capitalized-name op)])
            (format 
"
~a
#[derive(Copy, Clone)]
#[repr(C, packed)]
pub struct ~a {
    pub base: BaseInstruction,
~a
}
impl std::ops::Deref for ~a {
    type Target = BaseInstruction;
    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl std::ops::DerefMut for ~a {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

impl ~a {
    ~a
    ~a
    ~a
    ~a
    ~a
    ~a
    ~a
    ~a
}
"
              (opcode-documentation op)
              capitalized-name (opcode-members op)
              capitalized-name 
              capitalized-name
              capitalized-name 
              id 
              length-value 
              (opcode-constructors op)
              (opcode-setters op)
              (opcode-check-impl op)
              (opcode-emitter-impl op)
              (opcode-emitter op)
              (opcode-dumper op)
              )))
    
    (define (opcode-print-members op prefix f)
        (apply string-append (map*
            (lambda (arg)
                (format "~a~a~%" prefix arg))
            (map* f (opcode-args op)))))
    (define (opcode-print-args op f)
        (string-join (map* f (opcode-args op)) "\n"))
        ;(apply string-append (map* (lambda (arg)
        ;    (string-append (f arg) "\n")) (opcode-args op))))
    (define (opcode-members op)
        (format "~a ~a" (opcode-print-members op "    "argument-field) (metadata-field (opcode-metadata op) "    ")))
    
    (define (opcode-typed-args op)
        (string-join
            (map* argument-create-param (opcode-args op)) ""))
    (define (opcode-untyped-args op)
        (string-join 
            (map* (lambda (x) (symbol->string (argument-name x))) (opcode-args op)) ", "))
    (define (opcode-typed-ref-args op)
        (apply string-append 
            (map* argument-create-ref-param (opcode-args op))))
    (define (opcode-typed-mut-args op)
        (apply string-append 
            (map* argument-create-mut-param (opcode-args op))))

    (define (opcode-wide16 op) 'op_wide16)
    (define (opcode-wide32 op) 'op_wide32)
    (define (opcode-traits op) 'OpcodeTraits)
    (define (opcode-type-prefix op) 'Scm)
    (define (opcode-map*-fields-with-size op prefix size f)
        (let ([args (list (make-argument 'Self::OPCODE_ID 'OpcodeID 0))])
            (set! args (append args (opcode-args op)))
            (unless (metadata-empty? (opcode-metadata op))
                (set! args (cons (metadata-emitter-local (opcode-metadata op)) args)))
            (map* 
                (lambda (arg)
                    (format "~a~a"
                        prefix 
                        (f arg size)))
                args)))

    (define (opcode-map*-operands-with-size op prefix size f)
        (let ([args '()])
            (set! args (append args (opcode-args op)))
            (unless (metadata-empty? (opcode-metadata op))
                (set! args (cons (metadata-emitter-local (opcode-metadata op)) args)))
            (map* 
                (lambda (arg)
                    (format "~a~a"
                        prefix 
                        (f arg size)))
                args)))

    (define (load-from-stream arg index size)
        (cond 
            [(argument? arg) (argument-load-from-stream arg index size)]
            [(metadata? arg) (metadata-load-from-stream arg index size)]
            [else (assertion-violation 'load-from-stream "invalid argument type" arg)]))

    (define (opcode-constructors op)
        ; fields = (@args || []) + (@metadata.empty? ? [] : [@metadata])
        (let* (
            [args (opcode-args op)]
            [metadata (opcode-metadata op)]
            ;[fields args]
            [fields (append args (if (metadata-empty? metadata) '() (list metadata)))]
            [init (lambda (size)
                (if (null? fields)
                    "Self { base: Default::default() }"
                    (format 
                        "~%         Self { ~%base: Default::default(),~%~a~% }"
                        (apply string-append (map*-with-index 
                            (lambda (arg index)
                                (string-append (load-from-stream arg index size) ",\n"))
                            fields)))))])
            
            (format 
"
    pub unsafe fn new_narrow(stream: *const u8) -> Self { ~a }
    pub unsafe fn new_wide16(stream: *const u16) -> Self { ~a }
    pub unsafe fn new_wide32(stream: *const u32) -> Self { ~a }

    pub unsafe fn decode(stream: *const u8) -> Self {
        if *stream == OP_WIDE32 {
            return Self::new_wide32(
                stream.add(1 + OpcodeIDWidthBySize::<{OpcodeSize::Wide32}>::VALUE).cast());
        } else if *stream == OP_WIDE16 {
            return Self::new_wide16(
                stream.add(1 + OpcodeIDWidthBySize::<{OpcodeSize::Wide16}>::VALUE).cast());
        } else {
            return Self::new_narrow(
                stream.add(OpcodeIDWidthBySize::<{OpcodeSize::Narrow}>::VALUE))
        }
    }
"
            (init "OpcodeSize::Narrow")
            (init "OpcodeSize::Wide16")
            (init "OpcodeSize::Wide32"))))
    
    (define (opcode-setters op)
        (opcode-print-args op (lambda (a) (argument-setter a))))

    (define (generated-file name f)
        (let ([out (transcoded-port (open-file-output-port name (file-options no-fail)) (native-transcoder))])
            (f out)
            (close-output-port out)))

    (set! dsl-run 
        (lambda (options f)
            (define sections '())
            (define current-section '())
            (define namespaces '())

            (define (begin-section name config)
                (if (not (null? current-section))
                    (error 'begin-section "section already started"))
                (set! current-section (make-section name config)))
            
            (define (end-section name)
                (if (not (eq? name (section-name current-section)))
                    (error 'end-section "section name mismatch" name (section-name current-section)))

                (section-create-ids! current-section)
                (set! sections (cons current-section sections))
                (set! current-section '()))
            
            (define (op name config)
                (when (null? current-section)
                    (error 'op "no section started"))
                (section-add-opcode current-section name config))
            
            (define (op-group desc ops config)
                (when (null? current-section)
                    (error 'op-group "no section started"))
                (section-add-opcode-group current-section desc ops config))
            
            (define (for-each-opcode f)
                (for-each 
                    (lambda (section)
                        (for-each f (section-opcodes section)))
                    sections))
            (define (filter-opcodes pred)
                (let ([res '()])
                    (for-each-opcode 
                        (lambda (op)
                            (when (pred op)
                                (set! res (cons op res))))))
                    (reverse res))
            (define (num-opcodes)
                (let loop ([xs sections] [count 0])
                    (if (null? xs)
                        count
                        (loop (cdr xs) (+ count (length (section-opcodes (car xs))))))))
                
            (f begin-section end-section op op-group)
            (generated-file (assq* 'structs-file options)
              (lambda (out) 
                (for-each-opcode 
                    (lambda (op)
                        (display (opcode-struct op) out)))
                (display (dump-bytecode (section-opcodes (car sections))) out)
                (newline out)
                ))
                
            (generated-file (assq* 'macros-file options)
                (lambda (out) 
                    (for-each 
                        (lambda (section)
                            (display (section-helpers section (num-opcodes)) out))
                        sections)))
            )))


(load "./generator/def.scm")