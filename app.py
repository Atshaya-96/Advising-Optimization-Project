#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 22:26:03 2026

@author: p
"""
import streamlit as st
import streamlit.components.v1 as components

# 1. Page Configuration - Wide mode helps prevent Tableau clipping
st.set_page_config(
    layout="wide", 
    page_title="Advising Optimization Portfolio | Atshaya Suresh",
    initial_sidebar_state="collapsed"
)

# 2. Header Section
st.title("Advising Service Optimization & Planning Suite")
st.markdown("""
### 🎯 Project Impact: 90% Reduction in Student Wait Times
This portfolio demonstrates a mathematical approach to Institutional Research, 
optimizing advisor allocation to reduce student wait times from **12 days to 1.2 days**.
""")

st.divider()

# 3. Interactive Dashboards Section
# Using a larger height (1200) and scrolling=True to ensure full visibility
st.write("#### 📊 Dashboard 1: Baseline Demand & Sentiment Analysis")
dash_1_url = "https://public.tableau.com/views/Mendoza-ND/Dashboard1?:embed=y&:showVizHome=n"
components.iframe(dash_1_url, height=1100, scrolling=True)

st.divider()

st.write("#### 📊 Dashboard 2: Optimized Scenario Results")
dash_2_url = "https://public.tableau.com/views/Optimization_17767379821220/Dashboard1?:embed=y&:showVizHome=n"
components.iframe(dash_2_url, height=1100, scrolling=True)

# 4. Technical Documentation (Collapsible)
st.divider()
with st.expander("🛠️ View Technical Proof & R Code (Linear Programming)"):
    st.write("The underlying optimization logic was developed in R using the `ompr` package.")
    st.markdown(f"[**Click here to view the full GitHub Repository**](https://github.com/Atshaya-96/Advising-Optimization-Project)")
    
    st.code("""
    # Optimization Logic Example
    library(ompr)
    library(dplyr)
    
    # Mathematical Model for Advisor-Student Synchronization
    # model <- MIPModel() %>%
    #   add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
    #   set_objective(sum_expr(cost[i, j] * x[i, j], i = 1:n, j = 1:m), "min")
    
    # The full logic is available in the .Rmd file in the GitHub link above.
    """, language='r')

st.info("Built with Python & Streamlit | Data Analysis & Optimization Portfolio")