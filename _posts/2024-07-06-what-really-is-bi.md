---
title: "What really is B.I."
excerpt_separator: "<!--more-->"
categories:
  - Essays
tags:
  - Business Intelligence
  - Blog
---



B.I. is . . . uhh, Brinkman index? Brain Injury? Biological Indicator? I've waited 4 years to get enough experience to write this blog post.

<!--more-->

I'm writing this after spending just over four years now employed as a Business Intelligence Engineer. I used to be a data analyst (similar to a business analyst), and I still stumble when someone asks me "what do you do for work?" even though I can tell you what I do every day, month, or given year. BI is a job role unlike most in tech; it casts a net so wide across every job family in tech that, as a result, it has removed any thematic, ritualistic practices in the job family. In Software engineering, developers argue about what "framework" is best or what is the most idiomatic programming language for an application. Business Analysts contemplate life over Tableau or PowerBI and why Pie charts should never be used, only eaten. Data engineers are "Designing Data-Intensive Applications" over methods and practices that have evolved over decades with tightly coupled rules and principles. Data Scientists argue over data backpropagation and feature selection, and whether or not LLMs really are going to change the world as we know it. Project Managers argue over Scrum, Sprints, Jira or Asana. Product Managers just . . . argue. 
But in Business Intelligence, BI, there aren't any of these arguments, and now I understand why. 

>. . . BI is as diverse as software engineering, but with as much ritual as the first day of a startup.


Whereas software engineering has git, docker, terraform, and a myriad of other automation and pipeline tools; a data engineer has Redshift, Spark, EMR, Star Schemas; a data Analyst has Tableau and Excel; a data scientist has Sagemaker and LLMs; BI doesn't "have" any of those. BI uses all of those tools, but all of those tools aren't for BI. BI doesn't argue over these because BI has such breadth in scope that the feature trajectory of each individual tool is an oversight unless that tool is part of the BIEs specialization. Data analysts are driving the features in visualization software because visualization is their specialty, software engineers are driving the features in the latest programming language, and data engineers are driving features in Spark. 


With the advent of new and improving tools for software engineering, data engineering, analytics, and modeling, its becoming easier for people with virtually no experience to write working software, transform one billion rows of data, and even write an LLM even though they have been popularized only as of a few months ago. The same improvements have also made it easier for BIEs to pick up on new contols to solve "Business Intelligence". Where a data engineer may call spark their specialization, a BIE will refer to it as just one small tool in their toolbelt. BIEs don't have common specializations, but most BIEs, are pseudo-specialists in a certain area over the rest -- a specialist in software, data pipelines, dashboarding, modeling, or management. 


Being a successful BIE is about fitting into the gaps in the business to reveal aspects about the business that can be improved. In sufficiently complex systems and products, a BIE will appear as many job families to help build products to enable the business make better decisions -- thats the only goal of a BIE.  Some BIEs need to be experts in data analysis because solving the business problem requires manipulating and presenting data in a readable way; other BIEs need to cover data science more because solving the business problem requires modeling. In my four years I've become a specialist in data engineering over the other areas because, more often than not, solving the business problem has required building very reliable, robust data pipelines to enable better metrics and analytics. 

> a BIE will appear as many job families to help build products to enable the business make better decisions -- thats the only goal of a BIE.


As a result, the technical skill level and variety varies greatly between any given BIE. As a baseline, all BIEs can write SQL, run a linear regression, and plot a graph in excel, but all have large variations in other skills that are regularly used in their roles. Given the roles and responsibilities of BI can be so diverse, what can be done to become a successful BIE in a role that prioritizes breadth over depth? 


A successful BIE strategy requires a set of core principles to better guide the BI process and improve business outcomes. Work backwards from the "so that" -- we'll call this the "so that" principle. 


## The "so that" principle 

Every objective in BI should reach back to a business goal. The business goal follows from the "so that", and every discussion of any BI goal can be placed within the "so that" context. If the statement following "so that" does not end with a clearly aligned business goal then the goal is the wrong goal. Below are listed various "so that" statements. One statement can even have multiple, consecutive "so that"s. I've left some of these intentionally ambiguou so you can fill in and follow up with more "so that" statements. 


- This dashboard monitors weekly sales *so that* the sales team can adjust their weekly goals.
- This pipelines calculates the difference between prediction and actuals *so that* we can identify defects in modeling *so that* the modeling team can take action an improve the predictions so that
- This model classifies reviews into good and bad sentiments *so that* we can build software to summarize reviews for customers *so that* customers can spend less time determining if the product is a good fit for them *so that* the customer experience is improved because shoppers don't like wasting time looking at things they don't want. 


The intial problem I had was how to define BI, how do I explain to people what I do, and how does the BI role show value in what, on paper, looks ambiguous. The "so that" principle is an expression exactly thereof. I write pipelines so that . . . , I create metrics so that . . . , I make dashboards so that . . .

All of these "so that" statements that I have solved thus far have been different from one to the next, and many don't share nearly any similarity between the two. The next time someone asks me what I do, Im going to tell them that last "so that"-business problem. 

