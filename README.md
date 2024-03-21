# choice-criteria
Reupload 2022
This repository contains the comparative methods [application](https://mintdata.shinyapps.io/choice-criteria/).

## Methods
Lexicographic Method: This method prioritizes criteria in a specific order. If the first criterion doesn’t distinguish alternatives, it moves to the next one. Formula: Let $x_i$ represent the value of alternative $i$ for the $k$-th criterion. The lexicographic ranking is based on the order of criteria: $A \succ B$ if and only if $x_{A,k} > x_{B,k}$ for the first criterion $k$ where they differ.

Additive Method: In this approach, we assign weights to criteria and sum up the weighted scores for each alternative. Formula: The overall score for alternative $i$ is given by $\sum_{k=1}^{n} w_k \cdot x_{i,k}$, where $w_k$ is the weight for criterion $k$.

Laplace’s Method: Laplace assumes equal probabilities for outcomes and calculates the expected value. Formula: The expected value for alternative $i$ is $\frac{1}{n} \sum_{k=1}^{n} x_{i,k}$.

Dominance Method: This method compares alternatives based on dominance (superiority) in all criteria. Formula: Alternative $A$ dominates alternative $B$ if $x_{A,k} \geq x_{B,k}$ for all criteria $k$.

## Screenshot
![image](https://github.com/StellAuror/choice-criteria/assets/100155329/2cd3cd94-3c91-458f-b968-41d0b922e9ba)
![image](https://github.com/StellAuror/choice-criteria/assets/100155329/63417305-69f0-4411-ad26-0f5ab5b04d24)
