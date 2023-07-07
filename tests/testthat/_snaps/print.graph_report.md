# printing Bonferroni/Simes closure test

    Code
      test_graph_closure(par_gate, rep(0.01, 4), test_types = "s")
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        simes: (H1-H2-H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.01   TRUE
                H2         0.01   TRUE
                H3         0.01   TRUE
                H4         0.01   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  0  0
        H2  0  0  0  0
        H3  0  0  0  0
        H4  0  0  0  0

---

    Code
      test_graph_closure(par_gate, rep(0.01, 4), verbose = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  0  0
        H2  0  0  0  0
        H3  0  0  0  0
        H4  0  0  0  0
      
      Test details - Adjusted p ($details) -------------------------------------------
             H1   H2   H3   H4 adj_p_grp1 adj_p_inter reject
        1  0.50 0.50 0.00 0.00       0.02        0.02   TRUE
        2  0.50 0.50 0.00   NA       0.02        0.02   TRUE
        3  0.50 0.50   NA 0.00       0.02        0.02   TRUE
        4  0.50 0.50   NA   NA       0.02        0.02   TRUE
        5  0.50   NA 0.00 0.50       0.02        0.02   TRUE
        6  1.00   NA 0.00   NA       0.01        0.01   TRUE
        7  0.50   NA   NA 0.50       0.02        0.02   TRUE
        8  1.00   NA   NA   NA       0.01        0.01   TRUE
        9    NA 0.50 0.50 0.00       0.02        0.02   TRUE
        10   NA 0.50 0.50   NA       0.02        0.02   TRUE
        ... (Use `print(x, rows = <nn>)` for more)
      

---

    Code
      test_graph_closure(par_gate, rep(0.01, 4), critical = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  0  0
        H2  0  0  0  0
        H3  0  0  0  0
        H4  0  0  0  0
      Test details - Critical values ($critical) -------------------------------------
         Intersection Hypothesis       Test    p <= Critical * Alpha Reject
                    1         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                    1         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                    1         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                    1         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                    2         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                    2         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                    2         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                    3         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                    3         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                    3         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
        ... (Use `print(x, rows = <nn>)` for more)
      

# printing parametric closure test

    Code
      test_graph_closure(par_gate, rep(0.01, 4), test_types = "p", corr = diag(4))
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0  0  0
                            H2  0  1  0  0
                            H3  0  0  1  0
                            H4  0  0  0  1
      
        Test types
        parametric: (H1-H2-H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1       0.0199   TRUE
                H2       0.0199   TRUE
                H3       0.0199   TRUE
                H4       0.0199   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  0  0
        H2  0  0  0  0
        H3  0  0  0  0
        H4  0  0  0  0

---

    Code
      test_graph_closure(par_gate, rep(0.01, 4), groups = list(1:2, 3:4), test_types = c(
        "p", "s"), corr = diag(4), critical = TRUE, verbose = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0  0  0
                            H2  0  1  0  0
                            H3  0  0  1  0
                            H4  0  0  0  1
      
        Test types
        parametric: (H1-H2)
             simes: (H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  0  0
        H2  0  0  0  0
        H3  0  0  0  0
        H4  0  0  0  0
      
      Test details - Adjusted p ($details) -------------------------------------------
               H1     H2     H3     H4 adj_p_grp1 adj_p_grp2 adj_p_inter reject
        1  0.5000 0.5000 0.0000 0.0000     0.0199     1.0000      0.0199   TRUE
        2  0.5000 0.5000 0.0000     NA     0.0199     1.0000      0.0199   TRUE
        3  0.5000 0.5000     NA 0.0000     0.0199     1.0000      0.0199   TRUE
        4  0.5000 0.5000     NA     NA     0.0199     1.0000      0.0199   TRUE
        5  0.5000     NA 0.0000 0.5000     0.0200     0.0200      0.0200   TRUE
        6  1.0000     NA 0.0000     NA     0.0100     1.0000      0.0100   TRUE
        7  0.5000     NA     NA 0.5000     0.0200     0.0200      0.0200   TRUE
        8  1.0000     NA     NA     NA     0.0100     1.0000      0.0100   TRUE
        9      NA 0.5000 0.5000 0.0000     0.0200     0.0200      0.0200   TRUE
        10     NA 0.5000 0.5000     NA     0.0200     0.0200      0.0200   TRUE
        ... (Use `print(x, rows = <nn>)` for more)
      
      Test details - Critical values ($critical) -------------------------------------
         Intersection Hypothesis       Test    p <= c_value * Critical * Alpha Reject
                    1         H1 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    1         H2 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    1         H3      simes 0.01 <=      NA        0.0 * 0.025  FALSE
                    1         H4      simes 0.01 <=      NA        0.0 * 0.025  FALSE
                    2         H1 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    2         H2 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    2         H3      simes 0.01 <=      NA        0.0 * 0.025  FALSE
                    3         H1 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    3         H2 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    3         H4      simes 0.01 <=      NA        0.0 * 0.025  FALSE
        ... (Use `print(x, rows = <nn>)` for more)
      

---

    Code
      test_graph_closure(par_gate, rep(0.01, 4), groups = list(1:2, 3:4), test_types = c(
        "p", "p"), corr = diag(4), critical = TRUE, verbose = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0  0  0
                            H2  0  1  0  0
                            H3  0  0  1  0
                            H4  0  0  0  1
      
        Test types
        parametric: (H1-H2)
        parametric: (H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  0  0
        H2  0  0  0  0
        H3  0  0  0  0
        H4  0  0  0  0
      
      Test details - Adjusted p ($details) -------------------------------------------
               H1     H2     H3     H4 adj_p_grp1 adj_p_grp2 adj_p_inter reject
        1  0.5000 0.5000 0.0000 0.0000     0.0199     1.0000      0.0199   TRUE
        2  0.5000 0.5000 0.0000     NA     0.0199     1.0000      0.0199   TRUE
        3  0.5000 0.5000     NA 0.0000     0.0199     1.0000      0.0199   TRUE
        4  0.5000 0.5000     NA     NA     0.0199     1.0000      0.0199   TRUE
        5  0.5000     NA 0.0000 0.5000     0.0200     0.0200      0.0200   TRUE
        6  1.0000     NA 0.0000     NA     0.0100     1.0000      0.0100   TRUE
        7  0.5000     NA     NA 0.5000     0.0200     0.0200      0.0200   TRUE
        8  1.0000     NA     NA     NA     0.0100     1.0000      0.0100   TRUE
        9      NA 0.5000 0.5000 0.0000     0.0200     0.0200      0.0200   TRUE
        10     NA 0.5000 0.5000     NA     0.0200     0.0200      0.0200   TRUE
        ... (Use `print(x, rows = <nn>)` for more)
      
      Test details - Critical values ($critical) -------------------------------------
         Intersection Hypothesis       Test    p <= c_value * Critical * Alpha Reject
                    1         H1 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    1         H2 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    1         H3 parametric 0.01 <= 1.00000 *      0.0 * 0.025  FALSE
                    1         H4 parametric 0.01 <= 1.00000 *      0.0 * 0.025  FALSE
                    2         H1 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    2         H2 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    2         H3 parametric 0.01 <= 1.00000 *      0.0 * 0.025  FALSE
                    3         H1 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    3         H2 parametric 0.01 <= 1.00633 *      0.5 * 0.025   TRUE
                    3         H4 parametric 0.01 <= 1.00000 *      0.0 * 0.025  FALSE
        ... (Use `print(x, rows = <nn>)` for more)
      

# printing Bonferroni sequential results

    Code
      test_graph_shortcut(simple_successive_1(), rep(0.01, 4))
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  0  0
        H2  0  0  0  0
        H3  0  0  0  0
        H4  0  0  0  0

# additional printing options for graph report

    Code
      print(test_graph_closure(par_gate, rep(0.01, 4), verbose = TRUE, critical = TRUE),
      precison = 4, indent = 4)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
          Initial graph
      
          --- Hypothesis weights ---
          H1: 0.5
          H2: 0.5
          H3: 0.0
          H4: 0.0
      
          --- Transition weights ---
             H1 H2 H3 H4
          H1  0  0  1  0
          H2  0  0  0  1
          H3  0  1  0  0
          H4  1  0  0  0
      
          Alpha = 0.025
      
                                 H1   H2   H3   H4
          Unadjusted p-values: 0.01 0.01 0.01 0.01
      
          Test types
          bonferroni: (H1-H2-H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
          Hypothesis Adj. P-value Reject
                  H1         0.02   TRUE
                  H2         0.02   TRUE
                  H3         0.02   TRUE
                  H4         0.02   TRUE
      
          Updated graph after rejections
      
          --- Hypothesis weights ---
          H1: 0
          H2: 0
          H3: 0
          H4: 0
      
          --- Transition weights ---
             H1 H2 H3 H4
          H1  0  0  0  0
          H2  0  0  0  0
          H3  0  0  0  0
          H4  0  0  0  0
      
      Test details - Adjusted p ($details) -------------------------------------------
               H1   H2   H3   H4 adj_p_grp1 adj_p_inter reject
          1  0.50 0.50 0.00 0.00       0.02        0.02   TRUE
          2  0.50 0.50 0.00   NA       0.02        0.02   TRUE
          3  0.50 0.50   NA 0.00       0.02        0.02   TRUE
          4  0.50 0.50   NA   NA       0.02        0.02   TRUE
          5  0.50   NA 0.00 0.50       0.02        0.02   TRUE
          6  1.00   NA 0.00   NA       0.01        0.01   TRUE
          7  0.50   NA   NA 0.50       0.02        0.02   TRUE
          8  1.00   NA   NA   NA       0.01        0.01   TRUE
          9    NA 0.50 0.50 0.00       0.02        0.02   TRUE
          10   NA 0.50 0.50   NA       0.02        0.02   TRUE
          ... (Use `print(x, rows = <nn>)` for more)
      
      Test details - Critical values ($critical) -------------------------------------
           Intersection Hypothesis       Test    p <= Critical * Alpha Reject
                      1         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                      1         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                      1         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                      1         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                      2         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                      2         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                      2         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                      3         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                      3         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                      3         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
          ... (Use `print(x, rows = <nn>)` for more)
      

---

    Code
      print(test_graph_shortcut(simple_successive_1(), rep(0.01, 4), verbose = TRUE,
      critical = TRUE), precision = 7, indent = 9)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
               Initial graph
      
               --- Hypothesis weights ---
               H1: 0.5
               H2: 0.5
               H3: 0.0
               H4: 0.0
      
               --- Transition weights ---
                  H1 H2 H3 H4
               H1  0  0  1  0
               H2  0  0  0  1
               H3  0  1  0  0
               H4  1  0  0  0
      
               Alpha = 0.025
      
                                      H1   H2   H3   H4
               Unadjusted p-values: 0.01 0.01 0.01 0.01
      
               Test types
               bonferroni: (H1-H2-H3-H4)
      
      Test summary ($outputs) --------------------------------------------------------
               Hypothesis Adj. P-value Reject
                       H1         0.02   TRUE
                       H2         0.02   TRUE
                       H3         0.02   TRUE
                       H4         0.02   TRUE
      
               Updated graph after rejections
      
               --- Hypothesis weights ---
               H1: 0
               H2: 0
               H3: 0
               H4: 0
      
               --- Transition weights ---
                  H1 H2 H3 H4
               H1  0  0  0  0
               H2  0  0  0  0
               H3  0  0  0  0
               H4  0  0  0  0
      
      Test details - Rejection sequence ($details) -----------------------------------
               Initial graph
      
               --- Hypothesis weights ---
               H1: 0.5
               H2: 0.5
               H3: 0.0
               H4: 0.0
      
               --- Transition weights ---
                  H1 H2 H3 H4
               H1  0  0  1  0
               H2  0  0  0  1
               H3  0  1  0  0
               H4  1  0  0  0
      
                        Step 1: Delete hypothesis H1
      
                        --- Hypothesis weights ---
                        H1: 0.0
                        H2: 0.5
                        H3: 0.5
                        H4: 0.0
      
                        --- Transition weights ---
                           H1 H2 H3 H4
                        H1  0  0  0  0
                        H2  0  0  0  1
                        H3  0  1  0  0
                        H4  0  0  1  0
      
                                 Step 2: Delete hypothesis H2
      
                                 --- Hypothesis weights ---
                                 H1: 0.0
                                 H2: 0.0
                                 H3: 0.5
                                 H4: 0.5
      
                                 --- Transition weights ---
                                    H1 H2 H3 H4
                                 H1  0  0  0  0
                                 H2  0  0  0  0
                                 H3  0  0  0  1
                                 H4  0  0  1  0
      
                                          Step 3: Delete hypothesis H3
      
                                          --- Hypothesis weights ---
                                          H1: 0
                                          H2: 0
                                          H3: 0
                                          H4: 1
      
                                          --- Transition weights ---
                                             H1 H2 H3 H4
                                          H1  0  0  0  0
                                          H2  0  0  0  0
                                          H3  0  0  0  0
                                          H4  0  0  0  0
      
                                                   Step 4 (Ending state): Delete hypothesis H4
      
                                                   --- Hypothesis weights ---
                                                   H1: 0
                                                   H2: 0
                                                   H3: 0
                                                   H4: 0
      
                                                   --- Transition weights ---
                                                      H1 H2 H3 H4
                                                   H1  0  0  0  0
                                                   H2  0  0  0  0
                                                   H3  0  0  0  0
                                                   H4  0  0  0  0
      
      Test details - Critical values ($critical) -------------------------------------
                step Hypothesis    p <= Critical * Alpha Reject
                   1         H1 0.01 <=      0.5 * 0.025   TRUE
                   2         H2 0.01 <=      0.5 * 0.025   TRUE
                   3         H3 0.01 <=      0.5 * 0.025   TRUE
                   4         H4 0.01 <=      1.0 * 0.025   TRUE
      

---

    Code
      print(test_graph_shortcut(complex_example_1(), 5:0 / 200, verbose = TRUE,
      critical = TRUE))
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.0
        H3: 0.0
        H4: 0.5
        H5: 0.0
        H6: 0.0
      
        --- Transition weights ---
               H1     H2     H3     H4     H5     H6
        H1 0.0000 0.5000 0.5000 0.0000 0.0000 0.0000
        H2 0.0000 0.0000 1.0000 0.0000 0.0000 0.0000
        H3 0.0000 0.9999 0.0000 0.0001 0.0000 0.0000
        H4 0.0000 0.0000 0.0000 0.0000 0.5000 0.5000
        H5 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000
        H6 0.0001 0.0000 0.0000 0.0000 0.9999 0.0000
      
        Alpha = 0.025
      
                                H1    H2    H3    H4    H5    H6
        Unadjusted p-values: 0.025 0.020 0.015 0.010 0.005 0.000
      
        Test types
        bonferroni: (H1-H2-H3-H4-H5-H6)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1        0.025   TRUE
                H2        0.030  FALSE
                H3        0.030  FALSE
                H4        0.020   TRUE
                H5        0.020   TRUE
                H6        0.020   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0.0
        H2: 0.5
        H3: 0.5
        H4: 0.0
        H5: 0.0
        H6: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4 H5 H6
        H1  0  0  0  0  0  0
        H2  0  0  1  0  0  0
        H3  0  1  0  0  0  0
        H4  0  0  0  0  0  0
        H5  0  0  0  0  0  0
        H6  0  0  0  0  0  0
      
      Test details - Rejection sequence ($details) -----------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.0
        H3: 0.0
        H4: 0.5
        H5: 0.0
        H6: 0.0
      
        --- Transition weights ---
               H1     H2     H3     H4     H5     H6
        H1 0.0000 0.5000 0.5000 0.0000 0.0000 0.0000
        H2 0.0000 0.0000 1.0000 0.0000 0.0000 0.0000
        H3 0.0000 0.9999 0.0000 0.0001 0.0000 0.0000
        H4 0.0000 0.0000 0.0000 0.0000 0.5000 0.5000
        H5 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000
        H6 0.0001 0.0000 0.0000 0.0000 0.9999 0.0000
      
          Step 1: Delete hypothesis H4
      
          --- Hypothesis weights ---
          H1: 0.50
          H2: 0.00
          H3: 0.00
          H4: 0.00
          H5: 0.25
          H6: 0.25
      
          --- Transition weights ---
                  H1      H2      H3      H4      H5      H6
          H1 0.00000 0.50000 0.50000 0.00000 0.00000 0.00000
          H2 0.00000 0.00000 1.00000 0.00000 0.00000 0.00000
          H3 0.00000 0.99990 0.00000 0.00000 0.00005 0.00005
          H4 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
          H5 0.00000 0.00000 0.00000 0.00000 0.00000 1.00000
          H6 0.00010 0.00000 0.00000 0.00000 0.99990 0.00000
      
            Step 2: Delete hypothesis H6
      
            --- Hypothesis weights ---
            H1: 0.500025
            H2: 0.000000
            H3: 0.000000
            H4: 0.000000
            H5: 0.499975
            H6: 0.000000
      
            --- Transition weights ---
                        H1          H2          H3          H4          H5
            H1 0.000000000 0.500000000 0.500000000 0.000000000 0.000000000
            H2 0.000000000 0.000000000 1.000000000 0.000000000 0.000000000
            H3 0.000000005 0.999900000 0.000000000 0.000000000 0.000099995
            H4 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
            H5 1.000000000 0.000000000 0.000000000 0.000000000 0.000000000
            H6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
                H6
       0.000000000
       0.000000000
       0.000000000
       0.000000000
       0.000000000
       0.000000000
      
              Step 3: Delete hypothesis H5
      
              --- Hypothesis weights ---
              H1: 1
              H2: 0
              H3: 0
              H4: 0
              H5: 0
              H6: 0
      
              --- Transition weights ---
                     H1     H2     H3     H4     H5     H6
              H1 0.0000 0.5000 0.5000 0.0000 0.0000 0.0000
              H2 0.0000 0.0000 1.0000 0.0000 0.0000 0.0000
              H3 0.0001 0.9999 0.0000 0.0000 0.0000 0.0000
              H4 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
              H5 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
              H6 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
      
                Step 4 (Ending state): Delete hypothesis H1
      
                --- Hypothesis weights ---
                H1: 0.0
                H2: 0.5
                H3: 0.5
                H4: 0.0
                H5: 0.0
                H6: 0.0
      
                --- Transition weights ---
                   H1 H2 H3 H4 H5 H6
                H1  0  0  0  0  0  0
                H2  0  0  1  0  0  0
                H3  0  1  0  0  0  0
                H4  0  0  0  0  0  0
                H5  0  0  0  0  0  0
                H6  0  0  0  0  0  0
      
      Test details - Critical values ($critical) -------------------------------------
         step Hypothesis     p <= Critical * Alpha Reject
            1         H4 0.010 <= 0.500000 * 0.025   TRUE
            2         H6 0.000 <= 0.250000 * 0.025   TRUE
            3         H5 0.005 <= 0.499975 * 0.025   TRUE
            4         H1 0.025 <= 1.000000 * 0.025   TRUE
           NA         H3 0.015 <= 0.500000 * 0.025  FALSE
           NA         H2 0.020 <= 0.500000 * 0.025  FALSE
      
