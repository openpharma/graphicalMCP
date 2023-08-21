# printing Bonferroni power - sequential & closure

    Code
      graph_calculate_power(g, sim_seed = 51223)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 1
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
            H1  H2  H3  H4
        H1 0.0 0.5 0.5 0.0
        H2 0.0 0.0 0.0 1.0
        H3 0.0 0.5 0.0 0.5
        H4 0.0 1.0 0.0 0.0
      
        Alpha = 0.025
      
        Test types
        bonferroni: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
        Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                           H1    H2    H3    H4
        Marginal power: 0.025 0.025 0.025 0.025
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                      H1   H2   H3   H4
                       Local power: 0.02 0.00 0.00 0.00
      
        Expected no. of rejections: 0.02
         Power to reject 1 or more: 0.02
               Power to reject all: 0
      

---

    Code
      print(graph_calculate_power(g, sim_seed = 51223), indent = 6, precision = 3)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 1
            H2: 0
            H3: 0
            H4: 0
      
            --- Transition weights ---
                H1  H2  H3  H4
            H1 0.0 0.5 0.5 0.0
            H2 0.0 0.0 0.0 1.0
            H3 0.0 0.5 0.0 0.5
            H4 0.0 1.0 0.0 0.0
      
            Alpha = 0.025
      
            Test types
            bonferroni: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                               H1    H2    H3    H4
            Marginal power: 0.025 0.025 0.025 0.025
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                          H1   H2   H3   H4
                           Local power: 0.02 0.00 0.00 0.00
      
            Expected no. of rejections: 0.02
             Power to reject 1 or more: 0.02
                   Power to reject all: 0
      

---

    Code
      print(graph_calculate_power(g, sim_seed = 51223, force_closure = TRUE), indent = 6,
      precision = 3)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 1
            H2: 0
            H3: 0
            H4: 0
      
            --- Transition weights ---
                H1  H2  H3  H4
            H1 0.0 0.5 0.5 0.0
            H2 0.0 0.0 0.0 1.0
            H3 0.0 0.5 0.0 0.5
            H4 0.0 1.0 0.0 0.0
      
            Alpha = 0.025
      
            Test types
            bonferroni: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                               H1    H2    H3    H4
            Marginal power: 0.025 0.025 0.025 0.025
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                          H1   H2   H3   H4
                           Local power: 0.02 0.00 0.00 0.00
      
            Expected no. of rejections: 0.02
             Power to reject 1 or more: 0.02
                   Power to reject all: 0
      

# printing Simes power

    Code
      graph_calculate_power(g, sim_seed = 51223, test_types = "s")
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
      
        Test types
        simes: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
        Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                           H1    H2    H3    H4
        Marginal power: 0.025 0.025 0.025 0.025
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                      H1   H2   H3   H4
                       Local power: 0.01 0.01 0.00 0.00
      
        Expected no. of rejections: 0.02
         Power to reject 1 or more: 0.02
               Power to reject all: 0
      

---

    Code
      print(graph_calculate_power(g, sim_seed = 51223, test_types = "s"), indent = 6,
      precision = 3)
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
      
            Test types
            simes: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                               H1    H2    H3    H4
            Marginal power: 0.025 0.025 0.025 0.025
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                          H1   H2   H3   H4
                           Local power: 0.01 0.01 0.00 0.00
      
            Expected no. of rejections: 0.02
             Power to reject 1 or more: 0.02
                   Power to reject all: 0
      

# printing parametric power

    Code
      graph_calculate_power(g, sim_seed = 51223, test_types = "p", test_corr = list(
        diag(4)))
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 1
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  1  0  0
        H2  0  0  1  0
        H3  0  0  0  1
        H4  0  0  0  0
      
        Alpha = 0.025
      
        Parametric testing correlation:    H1 H2 H3 H4
                                        H1  1  0  0  0
                                        H2  0  1  0  0
                                        H3  0  0  1  0
                                        H4  0  0  0  1
      
        Test types
        parametric: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
        Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                           H1    H2    H3    H4
        Marginal power: 0.025 0.025 0.025 0.025
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                      H1   H2   H3   H4
                       Local power: 0.02 0.00 0.00 0.00
      
        Expected no. of rejections: 0.02
         Power to reject 1 or more: 0.02
               Power to reject all: 0
      

---

    Code
      print(graph_calculate_power(g, sim_seed = 51223, test_types = "p", test_corr = list(
        diag(4))), indent = 6, precision = 3)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 1
            H2: 0
            H3: 0
            H4: 0
      
            --- Transition weights ---
               H1 H2 H3 H4
            H1  0  1  0  0
            H2  0  0  1  0
            H3  0  0  0  1
            H4  0  0  0  0
      
            Alpha = 0.025
      
            Parametric testing correlation:    H1 H2 H3 H4
                                            H1  1  0  0  0
                                            H2  0  1  0  0
                                            H3  0  0  1  0
                                            H4  0  0  0  1
      
            Test types
            parametric: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                               H1    H2    H3    H4
            Marginal power: 0.025 0.025 0.025 0.025
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                          H1   H2   H3   H4
                           Local power: 0.02 0.00 0.00 0.00
      
            Expected no. of rejections: 0.02
             Power to reject 1 or more: 0.02
                   Power to reject all: 0
      

# printing blended power

    Code
      print(graph_calculate_power(g, 0.0254871, list(4:3, c(6, 1), c(2, 5)), c("b",
        "s", "p"), list(NA, NA, t_corr[c(2, 5), c(2, 5)]), 1328, pi / seq(0.3, 2.8,
        by = 0.5) / 11, s_corr, list(function(.) .[1] || .[5] || .[6], function(.) .[
        2] && (.[5] || .[6])), 51223), indent = 0, precision = 10)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
      Initial graph
      
      --- Hypothesis weights ---
      H1: 0.1666666667
      H2: 0.1666666667
      H3: 0.1666666667
      H4: 0.1666666667
      H5: 0.1666666667
      H6: 0.1666666667
      
      --- Transition weights ---
           H1  H2  H3  H4  H5  H6
       H1 0.0 0.2 0.2 0.2 0.2 0.2
       H2 0.2 0.0 0.2 0.2 0.2 0.2
       H3 0.2 0.2 0.0 0.2 0.2 0.2
       H4 0.2 0.2 0.2 0.0 0.2 0.2
       H5 0.2 0.2 0.2 0.2 0.0 0.2
       H6 0.2 0.2 0.2 0.2 0.2 0.0
      
      Alpha = 0.0254871
      
       Parametric testing correlation:              H2           H5
                                       H2 1.0000000000 0.7853981634
                                       H5 0.7853981634 1.0000000000
      
      Test types
      bonferroni: (H4, H3)
           simes: (H6, H1)
      parametric: (H2, H5)
      
      Simulation parameters ($inputs) ------------------------------------------------
      Testing 1,328 simulations - random seed 51223 & multivariate normal params:
      
                                H1           H2           H3           H4
      Marginal power: 0.9519977738 0.3569991652 0.2196917940 0.1586662956
                                H5           H6
      Marginal power: 0.1241736227 0.1019997615
      
       Correlation:              H1           H2           H3           H4
                    H1 1.0000000000 0.7853981634 0.7853981634 0.7853981634
                    H2 0.7853981634 1.0000000000 0.7853981634 0.7853981634
                    H3 0.7853981634 0.7853981634 1.0000000000 0.7853981634
                    H4 0.7853981634 0.7853981634 0.7853981634 1.0000000000
                    H5 0.7853981634 0.7853981634 0.7853981634 0.7853981634
                    H6 0.7853981634 0.7853981634 0.7853981634 0.7853981634
                 H5           H6
       0.7853981634 0.7853981634
       0.7853981634 0.7853981634
       0.7853981634 0.7853981634
       0.7853981634 0.7853981634
       1.0000000000 0.7853981634
       0.7853981634 1.0000000000
      
      Power calculation ($power) -----------------------------------------------------
                                             H1            H2            H3
                     Local power: 0.84036144578 0.20256024096 0.08960843373
                                             H4            H5            H6
                     Local power: 0.07304216867 0.06325301205 0.05346385542
      
      Expected no. of rejections: 1.322289157
       Power to reject 1 or more: 0.8403614458
             Power to reject all: 0.0406626506
      
                 Power to reject:    .[1] || .[5] || .[6]   .[2] && (.[5] || .[6])
                                            0.84036144578            0.07003012048
      
