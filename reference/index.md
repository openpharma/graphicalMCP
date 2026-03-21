# Package index

## Creating an initial graph

- [`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md)
  : Create the initial graph for a multiple comparison procedure

- [`print(`*`<initial_graph>`*`)`](https://openpharma.github.io/graphicalMCP/reference/print.initial_graph.md)
  :

  S3 print method for the class `initial_graph`

- [`plot(`*`<initial_graph>`*`)`](https://openpharma.github.io/graphicalMCP/reference/plot.initial_graph.md)
  :

  S3 plot method for class `initial_graph`

- [`as_initial_graph()`](https://openpharma.github.io/graphicalMCP/reference/as_graph.md)
  [`as_graphMCP()`](https://openpharma.github.io/graphicalMCP/reference/as_graph.md)
  [`as_igraph()`](https://openpharma.github.io/graphicalMCP/reference/as_graph.md)
  : Convert between graphicalMCP, gMCP, and igraph graph classes

## Updating a graph

- [`graph_update()`](https://openpharma.github.io/graphicalMCP/reference/graph_update.md)
  : Obtain an updated graph by updating an initial graphical after
  deleting hypotheses

- [`print(`*`<updated_graph>`*`)`](https://openpharma.github.io/graphicalMCP/reference/print.updated_graph.md)
  :

  S3 print method for the class `updated_graph`

- [`plot(`*`<updated_graph>`*`)`](https://openpharma.github.io/graphicalMCP/reference/plot.updated_graph.md)
  :

  S3 plot method for the class `updated_graph`

## Calculating hypothesis weights in a closure

- [`graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md)
  : Generate the weighting strategy based on a graphical multiple
  comparison procedure

## Testing a graphical multiple comparison procedure

- [`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
  : Perform closed graphical multiple comparison procedures

- [`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
  : Perform shortcut (sequentially rejective) graphical multiple
  comparison procedures

- [`print(`*`<graph_report>`*`)`](https://openpharma.github.io/graphicalMCP/reference/print.graph_report.md)
  :

  S3 print method for the class `graph_report`

- [`graph_rejection_orderings()`](https://openpharma.github.io/graphicalMCP/reference/graph_rejection_orderings.md)
  : Find alternate rejection orderings (sequences) for shortcut tests

- [`adjust_p_bonferroni()`](https://openpharma.github.io/graphicalMCP/reference/adjust_p.md)
  [`adjust_p_parametric()`](https://openpharma.github.io/graphicalMCP/reference/adjust_p.md)
  [`adjust_p_simes()`](https://openpharma.github.io/graphicalMCP/reference/adjust_p.md)
  [`adjust_p_hochberg()`](https://openpharma.github.io/graphicalMCP/reference/adjust_p.md)
  : Calculate adjusted p-values

- [`adjust_weights_parametric()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md)
  [`adjust_weights_simes()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md)
  [`adjust_weights_hochberg()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md)
  : Calculate adjusted hypothesis weights

## Power simulation

- [`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
  : Calculate power values for a graphical multiple comparison procedure

- [`print(`*`<power_report>`*`)`](https://openpharma.github.io/graphicalMCP/reference/print.power_report.md)
  :

  S3 print method for the class `power_report`

## Example graphs

- [`bonferroni()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`bonferroni_weighted()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`bonferroni_holm()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`bonferroni_holm_weighted()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`dunnett_single_step()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`dunnett_single_step_weighted()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`dunnett_closure_weighted()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`hochberg()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`hommel()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`huque_etal()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`fallback()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`fallback_improved_1()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`fallback_improved_2()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`fixed_sequence()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`sidak()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`simple_successive_1()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`simple_successive_2()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`two_doses_two_primary_two_secondary()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`three_doses_two_primary_two_secondary()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  [`random_graph()`](https://openpharma.github.io/graphicalMCP/reference/example_graphs.md)
  : Example graphs of commonly used multiple comparison procedures
