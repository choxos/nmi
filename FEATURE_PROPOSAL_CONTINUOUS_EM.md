# Feature Proposal: Continuous Effect Modifiers Support

**Proposal ID:** NMI-001  
**Author:** Ahmad Sofi-Mahmudi  
**Date:** 2025-01-21  
**Target Version:** 1.1.0  
**Priority:** High 🔴  
**Status:** Draft

---

## 📝 **Executive Summary**

This proposal outlines the implementation of continuous effect modifier support in the NMI package, addressing one of the most significant limitations identified in the current methodology. Currently, NMI only supports binary/categorical effect modifiers, but many clinically relevant effect modifiers (age, BMI, biomarker levels) are continuous.

---

## 🎯 **Objectives**

### **Primary Objectives**
1. Enable direct modeling of continuous effect modifiers in NMI
2. Implement robust discretization strategies when needed
3. Provide spline-based interpolation capabilities
4. Maintain computational efficiency and statistical validity

### **Secondary Objectives**
1. Support mixed networks (binary + continuous EMs)
2. Enable sensitivity analysis for discretization choices
3. Provide guidance for optimal discretization strategies
4. Maintain backward compatibility with existing binary EM functionality

---

## 🔍 **Current State & Limitations**

### **Current Implementation**
- Binary effect modifiers only: `EM ∈ {0, 1}`
- Requires manual discretization of continuous variables
- Risk of information loss through categorization
- Suboptimal use of available data granularity

### **Identified Problems**
1. **Information Loss**: Discretizing continuous variables loses valuable information
2. **Arbitrary Cutpoints**: Choice of discretization thresholds is often arbitrary
3. **Reduced Power**: Categorization typically reduces statistical power
4. **Clinical Relevance**: Many EMs are naturally continuous (age, BMI, lab values)

---

## 🛠 **Proposed Solution**

### **Three-Tiered Approach**

#### **Tier 1: Direct Continuous Modeling**
```r
# Proposed new syntax
NMI_interpolation_continuous(
  IPD = IPD_data,
  AgD = AgD_data, 
  x_vect = c(age = 65.5, bmi = 28.3),  # Continuous target values
  AgD_EM_cols = c('age_mean', 'bmi_mean'),
  IPD_EM_cols = c('age', 'bmi'),
  em_types = c('continuous', 'continuous'),  # Specify EM types
  interpolation_method = "spline"  # or "linear", "polynomial"
)
```

#### **Tier 2: Intelligent Discretization**
```r
# Adaptive discretization with optimization
NMI_interpolation_adaptive(
  # ... standard parameters ...
  discretization_method = "adaptive",
  n_bins = "optimal",  # Auto-optimize number of bins
  discretization_criteria = "information_preserved"
)
```

#### **Tier 3: Mixed Effect Modifier Support**
```r
# Networks with both binary and continuous EMs
NMI_interpolation_mixed(
  # ... standard parameters ...
  em_types = c('binary', 'continuous', 'continuous'),
  x_vect = list(
    smoking = 1,      # Binary: smoker
    age = 65.5,       # Continuous: years
    bmi = 28.3        # Continuous: kg/m²
  )
)
```

---

## 🏗 **Technical Implementation**

### **Core Components**

#### **1. Continuous Interpolation Engine**
```r
#' Continuous Effect Modifier Interpolation
#' 
#' @param continuous_ems Matrix of continuous effect modifier values
#' @param target_values Vector of target EM values for interpolation
#' @param method Interpolation method ("spline", "linear", "polynomial")
#' @param constraints List of constraints (monotonicity, bounds, etc.)
continuous_interpolation <- function(continuous_ems, target_values, 
                                   method = "spline", constraints = NULL) {
  
  switch(method,
    "spline" = spline_interpolation(continuous_ems, target_values, constraints),
    "linear" = linear_interpolation(continuous_ems, target_values),
    "polynomial" = polynomial_interpolation(continuous_ems, target_values),
    stop("Unknown interpolation method")
  )
}
```

#### **2. Adaptive Discretization**
```r
#' Optimize discretization strategy
#' 
#' @param continuous_data Vector of continuous EM values
#' @param outcome_data Associated outcome data
#' @param method Discretization optimization method
#' @param max_bins Maximum number of bins to consider
optimize_discretization <- function(continuous_data, outcome_data, 
                                  method = "information_preserved", 
                                  max_bins = 10) {
  
  # Try different numbers of bins
  bin_candidates <- 2:max_bins
  
  # Evaluate each discretization
  criteria_values <- map_dbl(bin_candidates, ~{
    discretized <- discretize_optimal(continuous_data, n_bins = .x)
    evaluate_discretization(discretized, outcome_data, method)
  })
  
  # Return optimal discretization
  optimal_bins <- bin_candidates[which.max(criteria_values)]
  discretize_optimal(continuous_data, n_bins = optimal_bins)
}
```

#### **3. Spline-Based Interpolation**
```r
#' Spline interpolation for continuous effect modifiers
#' 
#' @param study_data Study-level EM summaries
#' @param target_em Target EM value for interpolation
#' @param spline_type Type of spline ("natural", "cubic", "smoothing")
spline_interpolation <- function(study_data, target_em, 
                               spline_type = "natural") {
  
  # Fit spline model
  spline_model <- switch(spline_type,
    "natural" = splines::ns(study_data$em_values, df = 3),
    "cubic" = splines::bs(study_data$em_values, df = 4),
    "smoothing" = smooth.spline(study_data$em_values, study_data$effects)
  )
  
  # Predict at target value
  predict(spline_model, newdata = target_em)
}
```

### **Statistical Framework**

#### **Continuous EM Model Extension**
```
# Current binary model:
θ_jk = μ_j + d_1k + β₁·x₁,j + β₂·x₂,j + ... (binary x's)

# Proposed continuous extension:
θ_jk = μ_j + d_1k + f₁(x₁,j) + f₂(x₂,j) + ... 

where f_i(·) can be:
- Linear: f_i(x) = β_i·x
- Spline: f_i(x) = Σ β_ik·B_k(x)  (B_k = basis functions)
- Polynomial: f_i(x) = Σ β_ik·x^k
```

#### **Uncertainty Quantification**
- Propagate interpolation uncertainty through final estimates
- Bootstrap confidence intervals for discretization sensitivity
- Bayesian credible intervals incorporating all sources of uncertainty

---

## 📊 **Implementation Plan**

### **Phase 1: Core Infrastructure (Month 1)**
- [ ] Design continuous EM data structures
- [ ] Implement basic linear interpolation
- [ ] Create validation framework
- [ ] Set up unit tests

### **Phase 2: Advanced Methods (Month 2)**
- [ ] Implement spline interpolation
- [ ] Add polynomial interpolation options
- [ ] Develop adaptive discretization algorithms
- [ ] Create constraint handling system

### **Phase 3: Integration & Testing (Month 3)**
- [ ] Integrate with existing NMI workflow
- [ ] Comprehensive simulation studies
- [ ] Performance optimization
- [ ] Documentation and examples

### **Phase 4: Validation & Release (Month 4)**
- [ ] Real-world case studies
- [ ] Benchmark against existing methods
- [ ] User feedback incorporation
- [ ] Release preparation

---

## 🧪 **Validation Strategy**

### **Simulation Studies**
1. **Performance Comparison**
   - Continuous vs discretized approaches
   - Different interpolation methods
   - Various sample sizes and effect sizes

2. **Robustness Testing**
   - Outlier sensitivity
   - Missing data patterns
   - Model misspecification

3. **Real-Data Applications**
   - Oncology biomarker studies
   - Cardiovascular risk factor analysis
   - Age-stratified treatment effects

### **Benchmark Datasets**
- Simulated networks with known continuous relationships
- Real published network meta-analyses with continuous EMs
- Cross-validation against traditional discretization approaches

---

## 📈 **Expected Benefits**

### **Statistical Advantages**
- **Increased Power**: Better use of available information
- **Reduced Bias**: Avoid arbitrary discretization choices
- **Improved Precision**: More accurate interpolation at target values
- **Clinical Relevance**: Natural modeling of continuous relationships

### **Practical Benefits**
- **Flexibility**: Handle diverse EM types naturally
- **Automation**: Reduce manual discretization decisions
- **Sensitivity**: Built-in robustness checks
- **Interpretability**: Clearer continuous relationships

---

## ⚠️ **Risks & Mitigation**

### **Technical Risks**
1. **Computational Complexity**
   - *Risk*: Increased computation time
   - *Mitigation*: Efficient algorithms, parallel processing

2. **Overfitting**
   - *Risk*: Flexible models may overfit small networks
   - *Mitigation*: Regularization, cross-validation, conservative defaults

3. **Convergence Issues**
   - *Risk*: MCMC may struggle with complex continuous models
   - *Mitigation*: Robust initialization, adaptive sampling

### **Methodological Risks**
1. **Model Misspecification**
   - *Risk*: Wrong functional form assumptions
   - *Mitigation*: Multiple model options, diagnostic tools

2. **Extrapolation Concerns**
   - *Risk*: Unreliable predictions outside data range
   - *Mitigation*: Extrapolation warnings, bounds checking

---

## 💻 **API Design**

### **New Functions**
```r
# Main functions
NMI_interpolation_continuous()    # Direct continuous modeling
NMI_interpolation_adaptive()      # Adaptive discretization
NMI_interpolation_mixed()         # Mixed EM types

# Helper functions
optimize_discretization()         # Find optimal bins
spline_interpolation()           # Spline-based interpolation
continuous_diagnostic_plots()    # Diagnostic visualizations
em_type_detection()             # Automatic EM type detection
```

### **Enhanced Existing Functions**
```r
# Updated with continuous support
NMI_interpolation(
  # ... existing parameters ...
  em_types = c("binary", "continuous"),  # NEW
  interpolation_method = "spline",       # NEW
  discretization_auto = TRUE             # NEW
)
```

---

## 📚 **Documentation Requirements**

### **Vignettes**
1. **"Continuous Effect Modifiers in NMI"** - Comprehensive tutorial
2. **"Choosing Interpolation Methods"** - Method selection guidance
3. **"Mixed Effect Modifier Networks"** - Advanced applications

### **Help Documentation**
- Function documentation with mathematical details
- Parameter descriptions and valid ranges
- Examples with real and simulated data
- Performance and computational considerations

### **Theoretical Background**
- Mathematical framework documentation
- Comparison with discretization approaches
- Assumptions and limitations
- Best practice recommendations

---

## 🎯 **Success Criteria**

### **Technical Milestones**
- [ ] All unit tests pass (>95% coverage)
- [ ] Performance within 2x of binary EM methods
- [ ] Successful integration with existing workflow
- [ ] Comprehensive documentation complete

### **Validation Milestones**
- [ ] Simulation studies show improved accuracy
- [ ] Real-world case studies demonstrate utility
- [ ] Peer review feedback incorporated
- [ ] User acceptance testing passed

### **Quality Milestones**
- [ ] Code review completed
- [ ] Style guide compliance
- [ ] Error handling robust
- [ ] Backward compatibility maintained

---

**Next Steps**: Upon approval, begin Phase 1 implementation with core infrastructure development.

---

*This proposal will be refined based on stakeholder feedback and technical feasibility assessment.* 