use capy::{
    compiler::fix_letrec::Graph,
    vm::{options::VMOptions, scm_init},
};

fn main() {
    let opts = match VMOptions::parse() {
        Ok(opts) => opts,
        Err(err) => {
            eprintln!("Error: {}", err);
            std::process::exit(1);
        }
    };

    let mut mmtk = mmtk::MMTKBuilder::new();
    mmtk.set_option("plan", opts.gc_plan.as_ref());
    mmtk.set_option(
        "gc_trigger",
        &format!(
            "DynamicHeapSize:{},{}",
            opts.gc_min_heap_size, opts.gc_max_heap_size
        ),
    );
    mmtk.set_option("threads", "4");

    let _vm = scm_init(mmtk.build(), opts.gc_plan);

    let mut graph = Graph::new();

    let a = graph.add_empty_vertex();
    let b = graph.add_empty_vertex();
    let c = graph.add_empty_vertex();
    let d = graph.add_empty_vertex();
    let e = graph.add_empty_vertex();

    graph.add_edge(b, a);
    graph.add_edge(a, c);
    graph.add_edge(c, b);
    graph.add_edge(a, d);
    graph.add_edge(d, e);

    let sccs = graph.tarjan_sccs();

    for scc in sccs {
        println!("SCC: {:?}", scc);
    }
}
