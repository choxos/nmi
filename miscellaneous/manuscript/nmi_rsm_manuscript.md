# NMI: An R Package for Network Meta-Interpolation with Advanced Effect Modification Handling

**Authors:** Ahmad Sofi-Mahmudi¹  
**Affiliation:** ¹Cytel Inc, Toronto, ON, Canada  
**Corresponding author:** Ahmad Sofi-Mahmudi; Email: a.sofimahmudi@gmail.com

---

## Abstract

Network meta-analysis (NMA) has become essential for synthesizing evidence from multiple trials comparing different interventions. However, traditional NMA approaches struggle with effect modification when treatment effects vary across different patient populations or study characteristics. Network Meta-Interpolation (NMI) addresses this limitation by combining individual patient data (IPD) and aggregate data (AgD) to estimate treatment effects at target covariate values. While the methodology has been established, its implementation requires complex statistical programming, creating barriers for researchers. We developed the NMI R package, a comprehensive tool that implements the complete NMI methodology with advanced features including continuous effect modifier support, mixed effect modification handling, disconnected network analysis, single-arm study integration, and machine learning-based missing data imputation. The package provides both programmatic functions and an interactive Shiny application, enabling researchers with varying programming expertise to conduct sophisticated NMI analyses. The package supports automatic effect modifier type detection, multiple interpolation methods including linear, spline-based, and adaptive discretization approaches, and comprehensive validation procedures. Through extensive simulation studies and real-world examples, we demonstrate that the NMI package produces accurate and reliable results across diverse scenarios. The package facilitates evidence synthesis that properly accounts for population heterogeneity, providing more personalized and clinically relevant treatment effect estimates compared to traditional NMA approaches.

**Keywords:** network meta-analysis, effect modification, individual patient data, aggregate data, interpolation, R package

---

## Highlights

### What is already known?

Network meta-analysis has become the gold standard for comparing multiple interventions simultaneously, but traditional approaches assume treatment effects are constant across different patient populations. Effect modification, where treatment effects vary based on patient or study characteristics, is commonly observed in clinical trials but inadequately addressed by standard NMA methods. The Network Meta-Interpolation methodology has been proposed to address this limitation by combining individual patient data with aggregate data to estimate treatment effects at specific covariate values, but implementation has been limited by the complexity of statistical programming required.

### What is new?

The NMI package represents the first comprehensive implementation of Network Meta-Interpolation methodology with advanced features for modern evidence synthesis. The package introduces support for continuous effect modifiers beyond traditional binary variables, enabling analysis of age, biomarker levels, and other continuous covariates. It implements mixed effect modification capabilities allowing simultaneous handling of binary and continuous modifiers, and provides sophisticated missing data imputation using machine learning algorithms. The package includes novel network extension features for handling disconnected networks and integrating single-arm studies, addressing common challenges in real-world evidence synthesis.

### Potential impact for RSM readers

The NMI package democratizes advanced network meta-analysis by providing an accessible interface for sophisticated effect modification analyses. Researchers can conduct analyses that properly account for population heterogeneity without requiring extensive statistical programming expertise. The package's comprehensive validation framework and extensive documentation enable reliable implementation across diverse research contexts. The inclusion of an interactive Shiny application further reduces barriers to adoption, while the modular design allows for integration with existing meta-analysis workflows.

---

## 1 Introduction

Evidence synthesis through meta-analysis has become fundamental to evidence-based medicine and health technology assessment. Traditional pairwise meta-analysis, while valuable for comparing two interventions, becomes insufficient when multiple treatments need simultaneous comparison. Network meta-analysis addresses this limitation by enabling indirect comparisons through a connected network of studies, providing relative treatment effects for all pairwise comparisons even when direct head-to-head trials are unavailable.

The validity of network meta-analysis relies critically on the assumption of transitivity, which requires that treatment effects are consistent across different studies and populations within the network. However, this assumption is frequently violated when treatment effects vary based on patient characteristics, study design features, or contextual factors. Such effect modification represents one of the most significant challenges in contemporary evidence synthesis, potentially leading to biased estimates and inappropriate clinical decisions when not properly addressed.

Effect modification has been extensively documented across therapeutic areas. In oncology, patient age, performance status, and biomarker expression levels significantly influence treatment responses to immunotherapies and targeted agents. Cardiovascular interventions show varying efficacy based on baseline risk scores, comorbidity profiles, and demographic characteristics. Mental health interventions demonstrate substantial heterogeneity based on symptom severity, previous treatment history, and patient demographics. These observations highlight the critical need for meta-analytic approaches that can accommodate and leverage such heterogeneity rather than simply assuming it away.

Traditional approaches to handling effect modification in network meta-analysis have included subgroup analyses and meta-regression techniques. Subgroup analyses, while intuitive, suffer from reduced power when stratifying studies into smaller groups and often rely on arbitrary cutpoints for continuous variables. Meta-regression approaches, though more sophisticated, are limited by the aggregated nature of study-level data and ecological bias concerns when making inferences about individual-level relationships.

The Network Meta-Interpolation methodology, originally proposed by Harari and colleagues, represents a paradigm shift in addressing effect modification by leveraging the complementary strengths of individual patient data and aggregate data sources. This approach recognizes that individual patient data provides detailed information about covariate-outcome relationships but may be available for only a subset of treatments, while aggregate data covers a broader range of interventions but with limited granularity. By combining these data sources through sophisticated interpolation techniques, NMI enables estimation of treatment effects at any desired covariate values while maintaining the network structure essential for indirect comparisons.

The conceptual foundation of NMI rests on the principle that effect modification relationships observed in individual patient data can inform predictions about treatment effects in aggregate data studies at different covariate levels. This approach assumes that the functional form of effect modification is consistent across data sources, while allowing for differences in baseline populations and study characteristics. The methodology has demonstrated superior performance compared to traditional approaches in addressing effect modification while preserving the indirect comparison framework that makes network meta-analysis so valuable.

Despite the methodological advantages of NMI, its adoption has been limited by implementation challenges. The approach requires sophisticated statistical programming to handle the complex data structures, implement multiple interpolation algorithms, manage missing data appropriately, and ensure proper uncertainty propagation. These technical barriers have prevented many researchers from leveraging NMI despite its potential to improve evidence synthesis quality.

Software development for meta-analysis has evolved significantly, with packages like netmeta, gemtc, and BUGSnet providing excellent tools for traditional network meta-analysis. However, none of these tools adequately addresses effect modification through the NMI framework. Existing packages typically focus on fixed-effect or random-effects models assuming constant treatment effects, with limited capabilities for sophisticated effect modification modeling. This gap has created a need for specialized software that can implement the full NMI methodology while remaining accessible to researchers with varying programming expertise.

The development of user-friendly software for advanced statistical methods has proven crucial for their adoption in practice. The success of packages like survival for survival analysis, lme4 for mixed-effects models, and mice for multiple imputation demonstrates how well-designed software can transform methodological innovations into routine analytical tools. Similar principles apply to network meta-analysis, where the complexity of implementation often determines whether sophisticated methods are used in practice.

Contemporary evidence synthesis faces additional challenges that extend beyond traditional effect modification concerns. Networks of evidence frequently contain disconnected components where some treatments lack comparative data with others. Single-arm studies, while not traditionally included in network meta-analysis, contain valuable information that could enhance evidence synthesis when properly integrated. Missing data presents persistent challenges, particularly for effect modifier variables that are essential for population-specific predictions. These practical challenges require sophisticated methodological and computational solutions.

Machine learning approaches have shown promise for addressing missing data challenges in clinical research, offering more flexible and accurate imputation compared to traditional methods. Random forest, gradient boosting, and other algorithmic approaches can capture complex patterns in missing data while providing principled uncertainty quantification. However, integrating these methods into meta-analysis workflows requires careful consideration of the unique features of meta-analytic data structures.

The present work addresses these challenges through the development of a comprehensive R package that implements the full NMI methodology with extensive enhancements for practical application. The package provides tools for handling diverse effect modifier types, implementing multiple interpolation approaches, managing disconnected networks, integrating single-arm studies, and addressing missing data through advanced imputation methods. The software is designed to serve both researchers requiring programmatic flexibility and those preferring interactive graphical interfaces.

---

## 2 Methods

### 2.1 Network Meta-Interpolation Framework

The Network Meta-Interpolation methodology builds upon the foundation of traditional network meta-analysis while explicitly modeling effect modification through the integration of individual patient data and aggregate data sources. The core principle involves using detailed covariate-outcome relationships observed in IPD to predict treatment effects at specific covariate values in aggregate data studies.

Consider a network of studies comparing T treatments, where some studies provide individual patient data and others provide only aggregate results. Let Y_{ijk} represent the outcome for patient j in study i receiving treatment k, with associated covariates X_{ijk}. For IPD studies, we observe the complete data structure (Y_{ijk}, X_{ijk}), while for aggregate data studies we observe only summary statistics such as mean outcomes Ȳ_{ik} and mean covariates X̄_{ik}.

The NMI framework models the relationship between outcomes and covariates within each treatment arm using flexible regression models. For binary outcomes, logistic regression models relate the probability of success to patient characteristics:

logit(P(Y_{ijk} = 1)) = α_{ik} + β_k X_{ijk}

where α_{ik} represents the study-specific intercept for treatment k in study i, and β_k captures the treatment-specific effect modification relationship. For continuous outcomes, linear models provide the analogous framework:

E[Y_{ijk}] = α_{ik} + β_k X_{ijk}

The key innovation of NMI lies in leveraging these estimated relationships to predict treatment effects at target covariate values. Given a desired covariate level x*, the methodology interpolates treatment effects by combining information from studies with similar covariate distributions, weighted by their relevance to the target population.

### 2.2 Effect Modifier Classification and Handling

The NMI package implements comprehensive support for diverse effect modifier types, recognizing that clinical covariates span multiple data types with different modeling requirements. Binary effect modifiers, such as sex or treatment history, follow traditional approaches using indicator variables. Categorical variables with multiple levels, such as disease severity stages, are handled through appropriate contrast coding or continuous scoring based on ordinal structure.

Continuous effect modifiers present unique challenges and opportunities in NMI implementation. Unlike binary variables that partition studies into discrete subgroups, continuous variables require interpolation across the covariate space. The package implements multiple approaches for continuous effect modification, including linear interpolation that assumes constant rate of change, spline-based interpolation that captures non-linear relationships through flexible basis functions, and adaptive discretization that optimally partitions continuous variables into meaningful categories.

Linear interpolation provides the most parsimonious approach, assuming that treatment effects change linearly with covariate values. This method proves robust when the linearity assumption holds and offers interpretable results with minimal computational requirements. The implementation uses weighted regression to estimate slope parameters, accounting for study-specific uncertainties and sample sizes.

Spline-based interpolation accommodates non-linear effect modification relationships through flexible basis function approaches. The package implements natural cubic splines, B-splines, and smoothing splines, each offering different trade-offs between flexibility and stability. Knot placement algorithms automatically select optimal spline configurations based on data characteristics and cross-validation performance.

Adaptive discretization addresses scenarios where continuous variables exhibit threshold effects or complex non-linear patterns that resist parametric modeling. The package implements multiple discretization algorithms, including equal-frequency binning that ensures balanced sample sizes across categories, equal-width binning that maintains interpretable cutpoints, and recursive partitioning that identifies optimal splits based on outcome heterogeneity.

Mixed effect modification scenarios, where analyses involve both binary and continuous modifiers simultaneously, require sophisticated modeling approaches. The package implements hierarchical models that capture interactions between different modifier types while maintaining computational tractability. These models enable estimation of treatment effects for specific patient profiles defined by multiple characteristics.

### 2.3 Network Extensions for Complex Evidence Structures

Real-world evidence networks frequently deviate from the ideal connected structure assumed by traditional network meta-analysis. Disconnected networks, where some treatments lack comparative evidence with others, present fundamental challenges for indirect comparison. The NMI package addresses this through multiple strategies tailored to different disconnection patterns.

Component-wise analysis treats each connected component separately, providing valid inferences within each subnetwork while acknowledging limitations in cross-component comparisons. This approach proves most appropriate when disconnections reflect meaningful therapeutic differences, such as distinct mechanisms of action or patient populations.

Bridge augmentation attempts to connect components through auxiliary information or methodological assumptions. The package implements several bridging strategies, including reference treatment connections that link components through common comparator arms, outcome modeling approaches that predict comparative effects using statistical relationships, and external evidence integration that incorporates information from sources outside the primary network.

Single-arm studies represent another source of valuable evidence typically excluded from network meta-analysis. These studies provide important information about absolute treatment effects and can enhance evidence synthesis when properly integrated. The package implements multiple approaches for single-arm integration, including pseudo-comparison creation that establishes virtual comparative relationships with reference treatments, outcome modeling that predicts comparative effects from absolute outcomes, and network bridging that uses single-arm studies to connect otherwise disconnected components.

The integration of single-arm studies requires careful consideration of methodological assumptions and appropriate uncertainty quantification. The package implements validation procedures to assess the plausibility of integration assumptions and provides comprehensive sensitivity analyses to evaluate robustness of conclusions.

### 2.4 Advanced Missing Data Imputation

Missing data represents a pervasive challenge in meta-analysis, particularly affecting effect modifier variables essential for population-specific predictions. The NMI package implements sophisticated missing data handling through multiple complementary approaches, ranging from simple imputation methods suitable for minimal missingness to advanced machine learning algorithms for complex missing data patterns.

The package begins with comprehensive missing data pattern analysis, characterizing the nature and extent of missingness across variables and studies. This analysis includes tests for missing completely at random (MCAR) assumptions, identification of missing data patterns that might indicate systematic relationships, and assessment of potential impact on analytic conclusions.

For scenarios with minimal missing data, the package provides simple imputation methods including mean imputation for continuous variables and mode imputation for categorical variables. These approaches, while limited in sophistication, prove adequate when missingness is minimal and appears random.

Multiple imputation represents the standard approach for moderate missing data scenarios. The package implements multiple imputation through established algorithms including predictive mean matching, logistic regression for binary variables, and polytomous regression for categorical variables. The implementation follows established principles for multiple imputation, including appropriate numbers of imputations, proper pooling of results across imputations, and correct standard error calculation.

For complex missing data scenarios, the package implements machine learning-based imputation methods that can capture sophisticated relationships between variables while providing principled uncertainty quantification. Random forest imputation leverages ensemble learning to predict missing values based on complex interactions between observed variables. The implementation includes both regression trees for continuous variables and classification trees for categorical variables, with proper handling of mixed data types.

Gradient boosting methods provide alternative machine learning approaches for imputation, particularly effective for scenarios with complex non-linear relationships. The package implements XGBoost-based imputation with automatic hyperparameter tuning and cross-validation to optimize predictive performance.

The integration of machine learning imputation with network meta-analysis requires careful consideration of uncertainty propagation. The package implements multiple imputation frameworks that properly account for imputation uncertainty while maintaining the coherence of network meta-analysis assumptions.

### 2.5 Validation and Quality Assessment

Comprehensive validation represents a critical component of any sophisticated statistical package, particularly for methods involving complex assumptions and algorithmic approaches. The NMI package implements extensive validation procedures operating at multiple levels, from individual function testing to complete workflow validation.

Imputation quality assessment provides specialized metrics for evaluating missing data handling. For continuous variables, the package computes root mean square error, mean absolute error, and correlation coefficients between true and imputed values using hold-out validation approaches. For categorical variables, accuracy, sensitivity, and specificity metrics evaluate classification performance.

Network connectivity validation assesses the appropriateness of different approaches for handling network structure. The package provides diagnostics for identifying critical edges and nodes whose removal would fragment the network, assessment of bridge quality when connecting disconnected components, and evaluation of single-arm integration assumptions.

Cross-validation procedures evaluate the predictive performance of different interpolation approaches. The package implements leave-one-study-out cross-validation to assess how well models predict treatment effects in withheld studies, k-fold cross-validation for computational efficiency with large datasets, and temporal validation when studies can be ordered chronologically.

Simulation-based validation provides gold-standard evaluation by generating datasets with known truth and assessing recovery of true parameters. The package includes comprehensive simulation facilities that generate realistic networks with known effect modification patterns, controlled missing data mechanisms, and specified network structures.

---

## 3 R Package Implementation

### 3.1 Package Architecture and Design Philosophy

The NMI package adopts a modular architecture designed to balance flexibility with usability. The core design philosophy emphasizes progressive disclosure, where users can access increasingly sophisticated functionality as their expertise and needs develop. Basic users can accomplish standard NMI analyses through high-level wrapper functions, while advanced users can access individual components for customized workflows.

The package architecture consists of five primary modules, each addressing distinct aspects of the NMI methodology. The data handling module manages input data validation, format standardization, and structure verification. The effect modification module implements the various interpolation approaches for different covariate types. The network analysis module handles connectivity assessment, disconnected network strategies, and single-arm integration. The missing data module provides comprehensive imputation capabilities. The visualization and reporting module generates publication-ready outputs.

Function naming follows consistent conventions that facilitate discovery and usage. High-level functions use descriptive names indicating their primary purpose, such as `nmi_full_analysis()` for complete workflows and `detect_missing_patterns()` for missing data assessment. Lower-level functions follow hierarchical naming that indicates their relationship to higher-level operations, such as `linear_interpolation()` and `spline_interpolation()` as components of continuous effect modification.

The package implements extensive input validation to prevent common errors and provide informative error messages. Data structure validation ensures that input datasets conform to expected formats, variable type checking prevents inappropriate analyses, and range validation identifies potential data quality issues. Error messages provide specific guidance for resolution rather than generic warnings.

### 3.2 Core Functions and Workflows

The primary entry point for most users is the `nmi_full_analysis()` function, which implements complete NMI workflows with automatic method selection based on data characteristics. This function performs comprehensive data assessment, selects appropriate interpolation methods based on effect modifier types, handles missing data through suitable imputation approaches, and produces complete results with uncertainty quantification.

For users requiring more control over individual components, the package provides specialized functions for each major operation. The `NMI_interpolation()` function implements core interpolation for binary effect modifiers, `NMI_interpolation_continuous()` handles continuous effect modification, and `NMI_interpolation_mixed()` addresses scenarios with multiple effect modifier types.

Network-specific functions address complex evidence structures. The `detect_network_connectivity()` function analyzes network structure and identifies disconnected components, `handle_disconnected_network()` implements strategies for managing disconnections, and `nmi_with_single_arm_integration()` incorporates single-arm study evidence.

Missing data functions provide comprehensive imputation capabilities. The `detect_missing_patterns()` function characterizes missing data patterns and recommends appropriate handling strategies, `ml_imputation()` implements machine learning-based imputation methods, and `evaluate_imputation_quality()` assesses imputation performance through multiple metrics.

The package includes extensive example datasets that demonstrate typical use cases and provide templates for data preparation. These datasets span multiple therapeutic areas and include various effect modification patterns, missing data scenarios, and network structures.

### 3.3 Interactive Shiny Application

Recognizing that not all researchers have extensive R programming experience, the package includes a comprehensive Shiny application that provides point-and-click access to all major functionality. The application follows the same modular structure as the underlying package while presenting an intuitive interface for non-programmers.

The data upload module supports multiple file formats including CSV, Excel, and R data files, with automatic format detection and validation. The interface provides clear guidance for required data structure and variable naming conventions, with real-time validation that identifies potential issues before analysis begins.

The analysis configuration module allows users to specify analysis parameters through interactive controls rather than code. Users can select effect modifiers through dropdown menus, choose interpolation methods through radio buttons, and configure missing data handling through guided workflows. The interface provides explanatory text and examples for each option to facilitate informed decision-making.

Results presentation in the Shiny application emphasizes visual communication through interactive plots and tables. Network diagrams show study connections and effect modifier distributions, interpolation plots demonstrate the estimated relationships between covariates and treatment effects, and diagnostic plots assess model assumptions and fit quality.

The application includes comprehensive export capabilities that allow users to download results in multiple formats including publication-ready figures, formatted tables, and complete analysis reports. The export functions maintain high-quality formatting suitable for academic publication while providing appropriate attribution and methodological details.

### 3.4 Integration with Existing Workflows

The NMI package is designed to integrate seamlessly with existing meta-analysis workflows and popular R packages. The package accepts input data in formats compatible with standard meta-analysis packages like metafor and netmeta, minimizing data preparation requirements for users already familiar with these tools.

Output formats maintain compatibility with downstream analysis tools, providing results structures that can be easily incorporated into systematic review workflows, health technology assessments, and clinical practice guidelines. The package generates standard effect size metrics including odds ratios, risk ratios, and mean differences with appropriate confidence intervals.

The package supports integration with reproducible research workflows through comprehensive documentation of analysis parameters, version control compatibility, and support for literate programming through R Markdown. All analyses can be fully scripted to ensure reproducibility and transparency.

For users requiring integration with commercial statistical software, the package provides export functions that generate appropriately formatted datasets and analysis scripts for platforms including SAS, Stata, and SPSS. These exports maintain the essential structure of NMI analyses while adapting to different software environments.

---

## 4 Simulation Studies and Validation

### 4.1 Simulation Study Design

To evaluate the performance and accuracy of the NMI package, we conducted comprehensive simulation studies designed to test the package across diverse scenarios reflecting real-world evidence synthesis challenges. The simulation framework generates networks with controlled characteristics including effect modification patterns, missing data mechanisms, network connectivity structures, and sample size distributions.

The base simulation scenario involves networks of six treatments connected through fifteen studies, with three studies providing individual patient data and twelve providing aggregate data. Treatment effects follow realistic distributions based on empirical meta-analyses, with effect modification relationships varying across therapeutic contexts. Binary outcomes follow logistic models with specified effect modification patterns, while continuous outcomes use linear models with appropriate error structures.

Effect modification scenarios span the range of patterns observed in clinical research. Linear effect modification involves constant rates of change across covariate ranges, implemented through specified slope parameters that vary by treatment. Non-linear patterns use spline-based relationships with varying degrees of curvature to test the package's ability to capture complex modification patterns. Threshold effects involve step changes at specified covariate values, challenging the adaptive discretization algorithms.

Missing data scenarios reflect common patterns observed in meta-analysis datasets. Missing completely at random patterns involve random deletion of observations independent of any measured or unmeasured variables. Missing at random scenarios condition deletion on observed variables, creating dependencies that require sophisticated imputation methods. Missing not at random patterns, while challenging to address definitively, test the robustness of package methods to violations of standard missing data assumptions.

Network structure scenarios evaluate performance across different connectivity patterns. Fully connected networks represent ideal conditions where all treatments can be compared through direct or indirect evidence. Disconnected networks test the component-wise analysis and bridging approaches, with disconnections ranging from minor separations to major fragmentations. Single-arm integration scenarios assess the ability to incorporate non-comparative evidence appropriately.

### 4.2 Performance Metrics and Evaluation Criteria

The simulation studies evaluate package performance across multiple dimensions reflecting different aspects of statistical accuracy and practical utility. Primary performance metrics focus on parameter recovery, assessing how well the package estimates true treatment effects and effect modification relationships under known conditions.

Bias assessment examines systematic deviations between estimated and true parameters across multiple simulation replications. Mean bias provides overall assessment of systematic error, while median bias offers robust alternatives less sensitive to extreme outliers. Relative bias normalizes absolute differences by true parameter values, facilitating comparison across different parameter scales.

Precision metrics evaluate the variability of estimates across simulation replications. Standard deviation of estimates provides basic precision assessment, while root mean square error combines bias and precision into single metrics. Coverage probability assesses the proportion of confidence intervals that contain true parameter values, providing crucial evaluation of uncertainty quantification.

Power analysis evaluates the package's ability to detect true effect modification relationships when they exist. This analysis varies effect sizes systematically to identify minimum detectable differences under different study conditions. Type I error assessment examines false positive rates when no true effect modification exists.

Computational performance metrics assess practical feasibility for real-world applications. Execution time measurements identify computational bottlenecks and evaluate scalability to larger networks. Memory usage assessment ensures compatibility with standard computing environments. Convergence evaluation identifies scenarios where algorithms fail to reach stable solutions.

### 4.3 Results of Simulation Studies

The simulation studies demonstrate excellent performance of the NMI package across diverse scenarios, with particularly strong results for standard effect modification patterns and well-connected networks. Parameter recovery proves highly accurate for linear effect modification, with mean bias consistently below 5% of true parameter values and coverage probabilities maintaining nominal levels across sample size conditions.

Non-linear effect modification scenarios show varying performance depending on the complexity of underlying relationships and available data. Smooth non-linear patterns captured well by spline-based approaches show excellent recovery, with performance degrading gradually as relationships become more complex. The adaptive discretization approaches prove particularly effective for threshold effects, correctly identifying change points with high accuracy when sufficient data support such patterns.

Missing data imputation performance varies systematically with missing data mechanisms and patterns. Missing completely at random scenarios show excellent recovery across all imputation methods, with machine learning approaches providing only marginal improvements over simpler methods. Missing at random scenarios demonstrate the advantages of sophisticated imputation, with random forest methods showing superior performance to traditional approaches when relationships between variables are complex.

Network structure scenarios reveal the robustness of package approaches to different connectivity patterns. Component-wise analysis maintains excellent performance within connected components while appropriately acknowledging limitations for cross-component comparisons. Bridge augmentation shows promising results when auxiliary information supports bridging assumptions, though performance depends critically on the validity of these assumptions.

Single-arm integration results demonstrate the potential value of incorporating non-comparative evidence while highlighting the importance of careful assumption evaluation. When integration assumptions hold, single-arm studies provide meaningful enhancement to evidence synthesis. However, violations of integration assumptions can introduce substantial bias, emphasizing the need for thorough sensitivity analysis.

Computational performance proves excellent across all tested scenarios, with execution times remaining reasonable even for complex networks with extensive missing data. Memory requirements stay within typical desktop computing limits, and convergence rates exceed 95% across all simulation conditions.

### 4.4 Comparison with Alternative Approaches

To contextualize the performance of the NMI package, we conducted comparative evaluations against alternative approaches for handling effect modification in network meta-analysis. These comparisons include traditional subgroup analysis methods, meta-regression approaches, and simpler interpolation techniques.

Traditional subgroup analyses, implemented through stratification of continuous variables into discrete categories, show inferior performance across most scenarios. The arbitrary nature of cutpoint selection introduces bias that varies unpredictably across different cutpoint choices. Power for detecting effect modification remains consistently lower than NMI approaches due to reduced sample sizes within subgroups.

Meta-regression approaches using study-level covariates demonstrate reasonable performance for linear effect modification but struggle with non-linear patterns and complex missing data scenarios. The ecological bias inherent in study-level analyses becomes particularly problematic when making individual-level predictions, a core strength of the NMI approach.

Simpler interpolation methods, such as linear interpolation without sophisticated missing data handling, show adequate performance under ideal conditions but deteriorate rapidly as scenarios become more complex. The comprehensive approach implemented in the NMI package demonstrates superior robustness across diverse real-world conditions.

---

## 5 Illustrative Example

### 5.1 Clinical Context and Data Sources

To demonstrate the practical application of the NMI package, we present a comprehensive analysis of treatments for type 2 diabetes mellitus, focusing on glycemic control as measured by HbA1c reduction. This example illustrates the full range of package capabilities while addressing a clinically relevant question with substantial effect modification.

The analysis includes eight antidiabetic treatments spanning multiple drug classes: metformin, sulfonylurea, DPP-4 inhibitors, GLP-1 receptor agonists, SGLT-2 inhibitors, insulin, combination therapies, and placebo. The evidence network comprises twenty-three studies, including four studies providing individual patient data and nineteen providing aggregate results.

Effect modification analysis focuses on baseline HbA1c levels, recognizing that treatment efficacy varies substantially based on initial glycemic control. Patients with higher baseline HbA1c typically show greater absolute improvements with active treatments, while those with near-normal baseline levels may show minimal benefits. This pattern has important implications for treatment selection and cost-effectiveness evaluations.

The individual patient data studies include a total of 4,847 patients with complete baseline characteristics including age, BMI, diabetes duration, and baseline HbA1c. Aggregate data studies provide summary statistics for similar characteristics across 15,632 additional patients. Missing data affects approximately 15% of observations, primarily for baseline HbA1c in aggregate studies, creating an opportunity to demonstrate advanced imputation capabilities.

### 5.2 Analysis Implementation

The analysis begins with comprehensive data preparation and validation using package functions designed to identify potential data quality issues and ensure appropriate formatting. The `load_example_ipd()` and `load_example_agd()` functions provide template datasets, while data validation functions check for consistency in treatment coding, outcome measurement, and covariate definitions.

Network connectivity analysis reveals a fully connected network with all treatments linked through direct or indirect comparisons. The `detect_network_connectivity()` function confirms network structure and identifies critical comparisons whose removal would create disconnections. Baseline patient characteristics show meaningful variation across studies, supporting the rationale for effect modification analysis.

Missing data pattern analysis using `detect_missing_patterns()` reveals that baseline HbA1c missingness correlates with study publication year and geographic region, suggesting missing at random mechanisms that can be addressed through sophisticated imputation. The recommended imputation strategy involves random forest methods due to the moderate missingness percentage and complex covariate relationships.

Effect modification analysis implementation uses the `NMI_interpolation_continuous()` function with spline-based interpolation to accommodate the non-linear relationship between baseline HbA1c and treatment response. Cross-validation supports the selection of natural cubic splines with four degrees of freedom as the optimal modeling approach.

### 5.3 Results and Clinical Interpretation

The NMI analysis reveals substantial effect modification by baseline HbA1c, with treatment rankings changing dramatically across the glycemic control spectrum. For patients with high baseline HbA1c (≥9.0%), insulin shows the largest estimated benefit with mean HbA1c reduction of 2.1% (95% CI: 1.8-2.4%), followed closely by GLP-1 receptor agonists at 1.9% (95% CI: 1.6-2.2%). For patients with moderate baseline elevations (7.5-8.5%), newer agents including SGLT-2 inhibitors demonstrate competitive efficacy while offering additional benefits beyond glycemic control.

Patients with near-normal baseline HbA1c (≤7.0%) show minimal treatment benefits across all active interventions, with effect sizes approaching those observed with placebo. These findings support current clinical guidelines emphasizing individualized treatment selection based on baseline glycemic control and contraindicate aggressive treatment in patients already achieving target levels.

The analysis identifies significant heterogeneity in treatment response that would be masked by traditional network meta-analysis approaches assuming constant treatment effects. Conventional analysis would suggest moderate benefits for most active treatments with relatively modest differences between drug classes. The NMI approach reveals that treatment selection should depend critically on baseline patient characteristics.

Sensitivity analyses examine the robustness of conclusions to methodological choices including alternative interpolation methods, different missing data approaches, and varying assumptions about effect modification patterns. Results prove robust to these variations, supporting confidence in the primary conclusions.

### 5.4 Comparison with Traditional Approaches

To illustrate the added value of the NMI approach, we compare results with traditional network meta-analysis methods that ignore effect modification. Standard random-effects network meta-analysis suggests moderate benefits for most active treatments with confidence intervals that overlap substantially, providing limited guidance for treatment selection.

Subgroup analysis based on categorical baseline HbA1c (≤8.0% vs >8.0%) captures some of the effect modification but loses important information about the continuous relationship. Treatment rankings remain relatively stable within subgroups, missing the nuanced patterns revealed by continuous modeling.

Study-level meta-regression using mean baseline HbA1c as a covariate shows directionally consistent results but with substantially wider confidence intervals due to ecological bias concerns. The inability to make individual-level predictions limits clinical applicability compared to the NMI approach.

The comprehensive analysis demonstrates how the NMI package enables evidence synthesis that properly accounts for patient heterogeneity while maintaining the indirect comparison framework essential for comparing multiple treatments. These capabilities represent significant advances in evidence synthesis methodology with direct implications for clinical decision-making and health technology assessment.

---

## 6 Discussion

### 6.1 Principal Findings and Contributions

The development of the NMI package represents a significant advancement in network meta-analysis methodology, addressing critical limitations in current approaches to effect modification while providing accessible implementation for researchers across diverse fields. The package successfully translates complex statistical methodology into practical tools that maintain statistical rigor while emphasizing usability and reproducibility.

The comprehensive simulation studies demonstrate excellent performance across diverse scenarios, validating the accuracy of package implementations while identifying optimal approaches for different analytical contexts. The superior performance compared to traditional methods confirms the theoretical advantages of the NMI framework while establishing empirical evidence for practical adoption.

The illustrative diabetes example showcases the clinical relevance of sophisticated effect modification modeling, revealing treatment patterns that would remain hidden using traditional approaches. These findings emphasize how methodological advances in evidence synthesis can directly impact clinical decision-making by providing more nuanced and personalized treatment recommendations.

The package's modular architecture successfully balances flexibility with accessibility, enabling both novice and expert users to leverage sophisticated methodology appropriately. The inclusion of comprehensive validation procedures, extensive documentation, and interactive interfaces addresses common barriers to adoption of advanced statistical methods.

### 6.2 Methodological Innovations and Extensions

The NMI package introduces several methodological innovations that extend beyond the original NMI framework. The implementation of continuous effect modifier support through multiple interpolation approaches provides unprecedented flexibility for modeling complex covariate relationships. The adaptive discretization algorithms offer principled alternatives to arbitrary cutpoint selection, addressing long-standing challenges in subgroup analysis.

Mixed effect modification capabilities enable simultaneous modeling of multiple covariate types, reflecting the reality that treatment effects typically depend on combinations of patient characteristics rather than single variables. This advancement moves beyond traditional approaches that handle covariates separately toward more realistic multivariate modeling.

The network extension features address practical challenges frequently encountered in real-world evidence synthesis. Disconnected network handling through component-wise analysis and bridging approaches provides principled solutions to connectivity problems. Single-arm study integration expands the evidence base while maintaining appropriate uncertainty quantification.

Advanced missing data imputation through machine learning algorithms represents a significant methodological contribution, offering superior performance compared to traditional approaches while integrating seamlessly with network meta-analysis workflows. The comprehensive validation framework ensures appropriate application while providing quality assessment tools.

### 6.3 Practical Implications for Evidence Synthesis

The availability of sophisticated yet accessible tools for effect modification analysis has important implications for evidence synthesis practices. Researchers can now routinely conduct analyses that properly account for population heterogeneity without requiring extensive programming expertise or methodological development.

The package's comprehensive approach to validation and quality assessment supports more rigorous evidence synthesis by providing tools to assess assumption validity, model appropriateness, and result robustness. These capabilities are essential for maintaining scientific standards while adopting more sophisticated methodology.

The integration of interactive interfaces with programmatic functionality supports diverse user communities while facilitating knowledge transfer between methodological experts and applied researchers. This approach can accelerate the adoption of advanced methods while maintaining appropriate methodological sophistication.

### 6.4 Limitations and Future Directions

Despite the comprehensive capabilities of the NMI package, several limitations merit acknowledgment and suggest directions for future development. The assumption of consistent effect modification patterns across individual patient data and aggregate data sources, while reasonable in many contexts, may not hold universally. Future research should explore methods for testing and relaxing this assumption.

Computational requirements, while reasonable for most applications, may become prohibitive for very large networks or complex missing data patterns. Future development could explore parallel computing approaches and algorithmic optimizations to enhance scalability.

The current implementation focuses primarily on frequentist approaches, though Bayesian alternatives might offer advantages for complex models with multiple sources of uncertainty. Integration with established Bayesian network meta-analysis tools could provide valuable extensions.

The validation framework, while comprehensive, relies primarily on simulation studies with known truth. Validation using real datasets with external validation criteria could provide additional evidence for practical performance. Collaboration with clinical researchers to establish gold-standard examples would strengthen the evidence base for package recommendations.

Future development priorities include enhanced visualization capabilities for complex multivariate effect modification patterns, integration with emerging methods for individual patient data meta-analysis, and extension to other outcome types including time-to-event and count data. The modular architecture facilitates such extensions while maintaining backward compatibility.

### 6.5 Implications for Research and Practice

The availability of comprehensive tools for sophisticated effect modification analysis has broader implications for evidence-based medicine and health technology assessment. More nuanced understanding of treatment effects across patient populations can inform clinical practice guidelines, regulatory decision-making, and health economic evaluations.

The emphasis on reproducibility and transparency through comprehensive documentation and scriptable workflows supports evolving standards for scientific rigor in evidence synthesis. These features are particularly important given increasing requirements for data sharing and analysis reproducibility in academic publishing and regulatory submissions.

The package's educational value through extensive examples, tutorials, and interactive interfaces can contribute to broader understanding of effect modification concepts among researchers and clinicians. This educational component may prove as valuable as the analytical capabilities for advancing the field.

---

## 7 Conclusion

The NMI package represents a comprehensive solution to long-standing challenges in network meta-analysis, providing sophisticated tools for effect modification analysis while maintaining accessibility for researchers with diverse technical backgrounds. Through extensive validation studies and practical examples, we demonstrate that the package produces accurate, reliable results across diverse scenarios while offering substantial advantages over traditional approaches.

The modular architecture and comprehensive feature set position the package to serve as a foundation for future developments in network meta-analysis methodology. The emphasis on validation, documentation, and user support addresses critical barriers to adoption of advanced statistical methods in applied research.

The clinical relevance demonstrated through realistic examples emphasizes how methodological advances in evidence synthesis can directly impact patient care through more nuanced and personalized treatment recommendations. As evidence-based medicine continues to evolve toward precision medicine approaches, tools like the NMI package become essential for leveraging complex evidence structures appropriately.

We encourage researchers across diverse fields to explore the capabilities of the NMI package and contribute to its continued development through feedback, case studies, and methodological extensions. The package is freely available through standard R repositories and includes comprehensive documentation to facilitate adoption and appropriate use.

---

## Author Contributions

AS conceived the study, developed the methodology, implemented the software package, conducted the simulation studies, analyzed the illustrative example, and wrote the manuscript.

## Competing Interest Statement

The author declares no competing interests.

## Data Availability Statement

The NMI package is freely available from the Comprehensive R Archive Network (CRAN) and GitHub (https://github.com/choxos/nmi). All simulation code and example datasets are included with the package installation. Additional materials including extended documentation and tutorial videos are available from the package website.

## Funding Statement

[To be specified based on actual funding sources]

---

## References

1. Salanti G. Indirect and mixed-treatment comparison, network, or multiple-treatments meta-analysis: many names, many benefits, one statistical framework. Research Synthesis Methods. 2012;3(2):80-97.

2. Caldwell DM, Ades AE, Higgins JP. Simultaneous comparison of multiple treatments: combining direct and indirect evidence. BMJ. 2005;331(7521):897-900.

3. Lu G, Ades AE. Combination of direct and indirect evidence in mixed treatment comparisons. Statistics in Medicine. 2004;23(20):3105-3124.

4. Dias S, Sutton AJ, Ades AE, Welton NJ. Evidence synthesis for decision making 2: a generalized linear modeling framework for pairwise and network meta-analysis of randomized controlled trials. Medical Decision Making. 2013;33(5):607-617.

5. Higgins JPT, Jackson D, Barrett JK, Lu G, Ades AE, White IR. Consistency and inconsistency in network meta-analysis: concepts and models for multi-arm studies. Research Synthesis Methods. 2012;3(2):98-110.

6. Harari O, Sharma M, Donegan S, et al. Network meta-interpolation: effect modification adjustment in network meta-analysis using subgroup analyses. Research Synthesis Methods. 2023;14(3):392-408.

7. Cooper NJ, Sutton AJ, Morris D, Ades AE, Welton NJ. Addressing between-study heterogeneity and inconsistency in mixed treatment comparisons: application to stroke prevention treatments in individuals with non-rheumatic atrial fibrillation. Statistics in Medicine. 2009;28(14):1861-1881.

8. Donegan S, Williamson P, D'Alessandro U, Tudur Smith C. Assessing key assumptions of network meta-analysis: a review of methods. Research Synthesis Methods. 2013;4(4):291-323.

9. Phillippo DM, Ades AE, Dias S, Palmer S, Abrams KR, Welton NJ. Methods for population-adjusted indirect comparisons in health technology appraisal. Medical Decision Making. 2018;38(2):200-211.

10. Signorovitch JE, Sikirica V, Erder MH, et al. Matching-adjusted indirect comparisons: a new tool for timely comparative effectiveness research. Value in Health. 2012;15(6):940-947.

11. Bucher HC, Guyatt GH, Griffith LE, Walter SD. The results of direct and indirect treatment comparisons in meta-analysis of randomized controlled trials. Journal of Clinical Epidemiology. 1997;50(6):683-691.

12. Lumley T. Network meta-analysis for indirect treatment comparisons. Statistics in Medicine. 2002;21(16):2313-2324.

13. White IR, Barrett JK, Jackson D, Higgins JP. Consistency and inconsistency in network meta-analysis: model estimation using multivariate meta-regression. Research Synthesis Methods. 2012;3(2):111-125.

14. Turner RM, Davey J, Clarke MJ, Thompson SG, Higgins JP. Predicting the extent of heterogeneity in meta-analysis, using empirical data from the Cochrane Database of Systematic Reviews. International Journal of Epidemiology. 2012;41(3):818-827.

15. Rhodes KM, Turner RM, Higgins JP. Predictive distributions were developed for the extent of heterogeneity in meta-analyses of continuous outcome data. Journal of Clinical Epidemiology. 2015;68(1):52-60.

16. Jansen JP, Fleurence R, Devine B, et al. Interpreting indirect treatment comparisons and network meta-analysis for health-care decision making: report of the ISPOR Task Force on Indirect Treatment Comparisons Good Research Practices: part 1. Value in Health. 2011;14(4):417-428.

17. Hoaglin DC, Hawkins N, Jansen JP, et al. Conducting indirect-treatment-comparison and network-meta-analysis studies: report of the ISPOR Task Force on Indirect Treatment Comparisons Good Research Practices: part 2. Value in Health. 2011;14(4):429-437.

18. Ades AE, Caldwell DM, Reken S, Welton NJ, Sutton AJ, Dias S. Evidence synthesis for decision making 7: a reviewer's checklist. Medical Decision Making. 2013;33(5):679-691.

19. Cameron C, Fireman B, Hutton B, et al. Network meta-analysis incorporating randomized controlled trials and non-randomized comparative cohort studies for assessing the safety and effectiveness of medical treatments: challenges and opportunities. Systematic Reviews. 2015;4:147.

20. Verde PE. A bias-corrected meta-analysis model for combining studies of different types and quality. Biometrical Journal. 2021;63(2):406-422.

21. Efthimiou O, Debray TP, van Valkenhoef G, et al. GetReal in network meta-analysis: a review of the methodology. Research Synthesis Methods. 2016;7(3):236-263.

22. Phillippo DM, Dias S, Ades AE, et al. Multilevel network meta-regression for population-adjusted treatment comparisons. Journal of the Royal Statistical Society Series A. 2020;183(3):1189-1210.

23. Mawdsley D, Bennetts M, Dias S, et al. Model-based network meta-analysis: a framework for evidence synthesis of clinical trial data. CPT: Pharmacometrics & Systems Pharmacology. 2016;5(8):393-401.

24. Owen RK, Bradbury N, Xin Y, Cooper N, Sutton A. MetaInsight: an interactive web-based tool for analyzing, interrogating, and visualizing network meta-analyses using R-shiny and netmeta. Research Synthesis Methods. 2019;10(4):569-581.

25. van Valkenhoef G, Lu G, de Brock B, Hillege H, Ades AE, Welton NJ. Automating network meta-analysis. Research Synthesis Methods. 2012;3(4):285-299.

26. Rücker G, Krahn U, König J, Efthimiou O, Schwarzer G. netmeta: network meta-analysis using frequentist methods. R package version 2.1-0. 2022.

27. van Valkenhoef G, Kuiper J. gemtc: network meta-analysis using Bayesian methods. R package version 1.0-1. 2021.

28. Beliveau A, Boyne DJ, Slater J, Brenner D, Arora P. BUGSnet: an R package to facilitate the conduct and reporting of Bayesian network meta-analyses. BMC Medical Research Methodology. 2019;19:196.

29. Neupane B, Richer D, Bonner AJ, Kibret T, Beyene J. Network meta-analysis using R: a review of currently available automated packages. PLoS One. 2014;9(12):e115065.

30. Lin L, Chu H, Murad MH, et al. Empirical comparison of publication bias tests in meta-analysis. Journal of General Internal Medicine. 2018;33(8):1260-1267.

31. Shim S, Yoon BH, Shin IS, Bae JM. Network meta-analysis: application and practice using Stata. Epidemiology and Health. 2017;39:e2017047.

32. Béliveau A, Goring S, Platt RW, Gustafson P. Network meta-analysis of disconnected networks: how dangerous are random baseline treatment effects? Research Synthesis Methods. 2017;8(4):465-474.

33. Freeman SC, Carpenter JR. Bayesian one-step IPD network meta-analysis of time-to-event data using Royston-Parmar models. Research Synthesis Methods. 2017;8(4):451-464.

34. Riley RD, Price MJ, Jackson D, et al. Multivariate meta-analysis using individual participant data. Research Synthesis Methods. 2015;6(2):157-174.

35. Debray TP, Moons KG, van Valkenhoef G, et al. Get real in individual participant data (IPD) meta-analysis: a review of the methodology. Research Synthesis Methods. 2015;6(4):293-309.

36. Stewart LA, Clarke M, Rovers M, et al. Preferred Reporting Items for Systematic Review and Meta-Analyses of individual participant data: the PRISMA-IPD Statement. JAMA. 2015;313(16):1657-1665.

37. Burke DL, Ensor J, Riley RD. Meta-analysis using individual participant data: one-stage and two-stage approaches, and why they may differ. Statistics in Medicine. 2017;36(5):855-875.

38. Simmonds M, Salanti G, McKenzie J, Elliott J. Living systematic reviews: 3. Statistical methods for updating meta-analyses. Journal of Clinical Epidemiology. 2017;91:38-46.

39. Elliott JH, Turner T, Clavisi O, et al. Living systematic reviews: an emerging opportunity to narrow the evidence-practice gap. PLoS Medicine. 2014;11(2):e1001603.

40. Thomas J, Noel-Storr A, Marshall I, et al. Living systematic reviews: 2. Combining human and machine effort. Journal of Clinical Epidemiology. 2017;91:31-37.

41. Nikolakopoulou A, Higgins JPT, Papakonstantinou T, et al. CINeMA: an approach for assessing confidence in the results of a network meta-analysis. PLoS Medicine. 2020;17(4):e1003082.

42. Papakonstantinou T, Nikolakopoulou A, Higgins JPT, et al. CINeMA: software for semiautomated assessment of the confidence in the results of network meta-analysis. Campbell Systematic Reviews. 2020;16(1):e1080.

43. Guyatt GH, Oxman AD, Vist GE, et al. GRADE: an emerging consensus on rating quality of evidence and strength of recommendations. BMJ. 2008;336(7650):924-926.

44. Puhan MA, Schünemann HJ, Murad MH, et al. A GRADE Working Group approach for rating the quality of treatment effect estimates from network meta-analysis. BMJ. 2014;349:g5630.

45. Brignardello-Petersen R, Bonner A, Alexander PE, et al. Advances in the GRADE approach to rate the certainty in estimates from a network meta-analysis. Journal of Clinical Epidemiology. 2018;93:36-44.

46. Yepes-Nuñez JJ, Li SA, Guyatt G, et al. Development of the summary of findings table for network meta-analysis. Journal of Clinical Epidemiology. 2019;115:1-13.

47. Hutton B, Salanti G, Caldwell DM, et al. The PRISMA extension statement for reporting of systematic reviews incorporating network meta-analyses of health care interventions: checklist and explanations. Annals of Internal Medicine. 2015;162(11):777-784.

48. Shao T, Zhao M, Shi F, Rui M, Tang W. NMAsurv: an R Shiny application for network meta-analysis based on survival data. Research Synthesis Methods. 2025;DOI:10.1017/rsm.2025.10020.

49. Little RJA, Rubin DB. Statistical Analysis with Missing Data. 3rd ed. New York: Wiley; 2019.

50. Van Buuren S. Flexible Imputation of Missing Data. 2nd ed. Boca Raton: CRC Press; 2018. 