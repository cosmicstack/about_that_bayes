# Bayesian Statistics and Inference

## *16-Week Advanced Curriculum*

## Introduction

This curriculum integrates three key texts:

- Ben Lambert's "A Student's Guide to Bayesian Statistics" (1st Edition)
- Richard McElreath's "Statistical Rethinking" (2nd Edition)
- Andrew Gelman's "Bayesian Data Analysis" (3rd Edition)

The schedule follows a logical progression from foundations to advanced applications, with approximately 12-15 hours of work per week including lectures, reading, coding exercises, and projects.

https://github.com/rmcelreath/stat_rethinking_2024/tree/main

https://www.youtube.com/playlist?list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus

Created by self, Claude Deep Research and Gemini Deep Research.

---

## Part I: Foundations (Weeks 1-4)

### **Week 1: The Golem of Prague - Statistical Modeling and Bayesian Inference**

- **Goal:** Understand the "why" of Bayesian modeling and the basic mechanics of Bayesian updating.
- **Materials:**
    - **McElreath:** Lecture 1; Book Ch 1-2
    - **Lambert:** Ch 1-3
- **Deliberate Practice (15 hours):**
    - **Lectures (2.5 hrs):** Watch Lecture 1. Pause and re-watch segments that are unclear.
    - **Reading & Summarizing (5 hrs):** Read the assigned chapters. For each major concept (e.g., priors, likelihood, posterior, grid approximation), write a one-paragraph summary in your own words without looking at the text.
    - **Practice Problems (6 hrs):** Complete the Chapter 2 practice problems from McElreath's GitHub. First, try to solve them with pencil and paper. Then, write the R/Python code to implement the grid approximation for each.
    - **Feedback (1.5 hrs):** Compare your code and answers to the solutions. Identify and understand every difference.

### **Week 2: Small Worlds and Large Worlds - Building and Evaluating Models**

- **Goal:** Learn to use binomial and Poisson models and understand the principles of model checking.
- **Materials:**
    - **McElreath:** Lecture 2; Book Ch 3
    - **Lambert:** Ch 4-5
    - **Gelman:** Ch 1
- **Deliberate Practice (15 hours):**
    - **Lectures (2.5 hrs):** Watch Lecture 2.
    - **Reading & Synthesis (5 hrs):** Read the chapters. Create a small concept map linking priors, likelihoods, and posteriors for the binomial (globe tossing) model.
    - **Practice Problems (6 hrs):** Work through the Chapter 3 problems. For each, explicitly write out the model definition (the "story" of how the data came to be) before you start coding.
    - **Feedback (1.5 hrs):** Review solutions. Did your model "story" match the one implied by the solution? If not, why?

### **Week 3: Linear Models & Gaussian Distributions**

- **Goal:** Understand the Gaussian distribution as a core component of linear models and how to use it for inference.
- **Materials:**
    - **McElreath:** Lecture 3; Book Ch 4
    - **Lambert:** Ch 6
    - **Gelman:** Ch 2-3
- **Deliberate Practice (15 hours):**
    - **Lectures (2.5 hrs):** Watch Lecture 3.
    - **Reading & Re-derivation (5 hrs):** Read the chapters. Try to re-derive the formula for the posterior of a Gaussian model with a known variance from memory. Check your work.
    - **Practice Problems (6 hrs):** Complete Chapter 4 problems. Before running the code, sketch a graph of what you expect the posterior distributions for the parameters to look like.
    - **Feedback (1.5 hrs):** Compare your sketched posteriors to the plotted posteriors from your code. Analyze the differences.

### **Week 4: Categories and Curves - Linear Models with Categorical Predictors**

- **Goal:** Master the use of dummy variables and understand how to model non-linear relationships with polynomials.
- **Materials:**
    - **McElreath:** Lecture 4 & 5; Book Ch 5
- **Deliberate Practice (15 hours):**
    - **Lectures (2.5 hrs):** Watch Lectures 4 & 5.
    - **Reading & Application (5 hrs):** After reading, find a simple dataset online (e.g., from TidyTuesday) and formulate a research question that could be answered with a linear model using a categorical predictor.
    - **Practice Problems (6 hrs):** Work through Chapter 5 problems. For each, verbally explain the meaning of each parameter in the model (e.g., "β1 is the average difference in height between...").
    - **Feedback (1.5 hrs):** Review solutions. For one of the problems, write a brief, non-technical summary of the findings as if you were explaining it to a collaborator.

## **Part II: Model Building and Evaluation (Weeks 5-9)**

### **Week 5: Overfitting and Underfitting**

- **Goal:** Understand the concepts of deviance, information criteria (AIC/DIC/WAIC), and regularization.
- **Materials:**
    - **McElreath:** Lecture 6; Book Ch 6
    - **Gelman:** Ch 7
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (7.5 hrs):** Watch lecture and read chapters. Explain the difference between in-sample and out-of-sample deviance to a rubber duck (or a friend).
    - **Practice Problems (6 hrs):** Complete Chapter 6 problems. For each model comparison, justify your choice of the "best" model using the information criteria, but also comment on the practical significance of the differences.
    - **Feedback (1.5 hrs):** Review solutions. Focus on the interpretation of WAIC and what it tells you about the models.

### **Week 6: Interactions**

- **Goal:** Learn how to specify, interpret, and visualize interaction effects in linear models.
- **Materials:**
    - **McElreath:** Lecture 7; Book Ch 7
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (5 hrs):** Watch lecture and read the chapter.
    - **Practice & Visualization (8 hrs):** Work through Chapter 7 problems. For each interaction model, create an interaction plot (e.g., plotting the predicted outcome against one predictor at different values of the other predictor).
    - **Feedback & Interpretation (2 hrs):** Review solutions. Write a sentence interpreting the interaction effect for each problem. For example: "The effect of variable X on Y becomes stronger as variable Z increases."

### **Week 7: Markov Chain Monte Carlo (MCMC)**

- **Goal:** Develop an intuitive and practical understanding of how MCMC methods like Gibbs and Metropolis-Hastings work to approximate the posterior.
- **Materials:**
    - **McElreath:** Lecture 8 & 9; Book Ch 8-9
    - **Lambert:** Ch 7-8
    - **Gelman:** Ch 11-12
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (7.5 hrs):** Watch lectures and read chapters. Draw a diagram illustrating how the Metropolis algorithm explores a parameter space.
    - **Practice Problems (6 hrs):** Complete Chapter 9 problems. Pay close attention to the diagnostic plots (trace plots, Rhat, n_eff). For one model, intentionally use a bad prior or starting value and observe how it affects the chains.
    - **Feedback (1.5 hrs):** Compare your diagnostic interpretations with the text's explanations.

### **Week 8: Generalized Linear Models (GLMs)**

- **Goal:** Extend linear models to non-Gaussian outcomes using link functions, focusing on logistic and Poisson regression.
- **Materials:**
    - **McElreath:** Lecture 10; Book Ch 10
    - **Lambert:** Ch 11
    - **Gelman:** Ch 16
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (6 hrs):** Watch lecture and read chapters. For both logistic and Poisson regression, identify the three key components: the distribution, the linear model, and the link function.
    - **Practice Problems (7 hrs):** Work through Chapter 10 problems. For a logistic regression problem, practice converting log-odds back to probabilities to interpret the results.
    - **Feedback (2 hrs):** Review solutions. Check your interpretations. Are you correctly interpreting effects on the scale of the linear model (log-odds) or the outcome scale (probability)?

### **Week 9: Counts and Other Outcomes**

- **Goal:** Model count data with Poisson and Negative-Binomial models, and handle ordered categorical outcomes.
- **Materials:**
    - **McElreath:** Lecture 11; Book Ch 11
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (5 hrs):** Watch lecture and read the chapter.
    - **Practice Problems (8 hrs):** Work through Chapter 11 problems. Focus on the interpretation of the overdispersion parameter in negative-binomial models. For an ordered logistic model, interpret the cutpoints.
    - **Feedback (2 hrs):** Review solutions. Explain why a Poisson model might be inappropriate for a dataset that a negative-binomial model fits well.

## **Part III: Advanced Topics (Weeks 10-14)**

### **Week 10: Multilevel Models I**

- **Goal:** Understand the motivation for multilevel models and specify simple varying intercept models.
- **Materials:**
    - **McElreath:** Lecture 12; Book Ch 12
    - **Gelman:** Ch 15
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (6 hrs):** Watch lecture and read chapters. Explain the concepts of "pooling" (complete, no, and partial) using a simple, non-technical example.
    - **Practice Problems (7 hrs):** Work through Chapter 12 problems. For a varying intercepts model, plot the posterior distribution of the standard deviation of the intercepts (σalpha) and interpret it.
    - **Feedback (2 hrs):** Review solutions. What does the value of σalpha tell you about the variation between groups?

### **Week 11: Multilevel Models II**

- **Goal:** Specify and interpret varying slope and varying slope-intercept models.
- **Materials:**
    - **McElreath:** Lecture 13; Book Ch 13
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (6 hrs):** Watch lecture and read the chapter.
    - **Practice Problems (7 hrs):** Work through Chapter 13 problems. Create plots showing the estimated regression line for each group in a varying slopes model.
    - **Feedback (2 hrs):** Review solutions. Interpret the correlation between varying intercepts and varying slopes. What does it mean if the correlation is negative?

### **Week 12: Missing Data and Measurement Error**

- **Goal:** Learn to incorporate missing data and measurement error directly into a Bayesian model.
- **Materials:**
    - **McElreath:** Lecture 14; Book Ch 14
    - **Gelman:** Ch 18
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (6 hrs):** Watch lecture and read chapters. Differentiate between MCAR, MAR, and MNAR.
    - **Practice (7 hrs):** Take a standard dataset from a previous chapter and introduce some missing values artificially. Use the techniques from the chapter to impute them and compare the results to the original, complete-data analysis.
    - **Feedback (2 hrs):** Reflect on how the imputation affected the posterior distributions.

### **Week 13: Gaussian Processes**

- **Goal:** Understand how to model complex non-linear relationships and spatial/temporal dependencies using Gaussian Processes.
- **Materials:**
    - **McElreath:** Lecture 15; Book Ch 15
- **Deliberate Practice (15 hours):**
    - **Lectures & Reading (7.5 hrs):** Watch lecture and read the chapter. Explain the role of the covariance function (kernel) in a GP.
    - **Practice (6 hrs):** Work through the Chapter 15 problems. Experiment with changing the parameters of the covariance function and observe how it changes the fitted curve.
    - **Feedback (1.5 hrs):** Review solutions. Develop an intuition for how the length-scale and amplitude parameters of the kernel affect the model's flexibility.

### **Week 14: Advanced Topics & Review**

- **Goal:** Consolidate knowledge and explore an advanced topic of interest.
- **Materials:**
    - **McElreath:** Lecture 16 (or other advanced lectures)
    - **Choose one:** Causal Inference (Ch 16), Social Networks, etc.
- **Deliberate Practice (15 hours):**
    - **Explore & Synthesize (15 hrs):** Choose a topic. Read the relevant chapter(s). Find a dataset and try to apply the methods. Write a short (2-3 page) report summarizing your model, results, and interpretation.

## **Part IV: Synthesis and Application (Weeks 15-16)**

### **Week 15: Project - Proposal and Model Building**

- **Goal:** Define a research question and dataset for a final project, and build a first version of your model.
- **Deliberate Practice (15 hours):**
    - **Find Data & Question (5 hrs):** Find a dataset relevant to your own field of study. Formulate a clear research question.
    - **Exploratory Analysis (5 hrs):** Visualize the data. What are the key patterns? What kind of model seems appropriate?
    - **Initial Model (5 hrs):** Build and run a first-pass model. This could be a simple linear or generalized linear model. Check the chains and interpret the initial results.

### **Week 16: Project - Iteration and Reporting**

- **Goal:** Refine your model, interpret the results, and communicate your findings.
- **Deliberate Practice (15 hours):**
    - **Model Iteration (7.5 hrs):** Based on Week 15, improve your model. Do you need interactions? Varying effects? A different likelihood? Use model comparison techniques (WAIC) to justify your choices.
    - **Reporting (7.5 hrs):** Write up a complete report of your project (~5-8 pages). Structure it like a short academic paper: Introduction, Methods (describe your model in detail), Results (plots and interpretations), and Discussion/Conclusion.