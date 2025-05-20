#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan 13 20:24:14 2025

@author: ASUS
"""

import numpy as np 
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

veg_soil_water = pd.read_excel("/Users/ASUS/Desktop/Machine_learning_project/veg_soil_water_measurements.xlsx")
veg_soil_water = veg_soil_water.drop(columns = "notes")
veg_soil_water.columns

data = veg_soil_water
data.info()

## Checking variable types

data.dtypes

# Converting continuous variables to float
columns_to_convert = ['Acetamiprid', 'Aminopyralid', 'Azoxystrobin', 'Benalaxyl',
       'Bentazone', 'Bixafen', 'Boscalid', 'Carfentrazone-ethyl',
       'Chlorantraniliprole', 'Chloridazon', 'Chlortoluron', 'Clomazone',
       'Clothianidin', 'Cyantraniliprole', 'Cyazofamid', 'Cyflufenamid',
       'Cymoxanil', 'Cyprodinil', 'Difenconazole', 'Dimethenamid-P',
       'Dimethoate', 'Dimethomorph', 'Dimoxystrobin', 'Epoxiconazole',
       'Etofenprox', 'Fenoxycarb', 'Fenpropimorph', 'Fenpyroximate',
       'Florasulam', 'Fludioxonil', 'Flufenacet', 'Fluopicolid', 'Fluopyram',
       'Flupyradifurone', 'Flurtamone', 'Hexythiazox', 'Imidacloprid',
       'Iprovalicarb', 'Isoproturon', 'Kresoxim methyl', 'Mandipropamid',
       'Metalaxyl', 'Metamitron', 'Metazachlor', 'Metconazole', 'Methiocarb',
       'Metobromuron', 'Metolachlor-S', 'Metrafenone', 'Metsulfuron-methyl',
       'Myclobutanil', 'Napropamide', 'Paclobutrazol', 'Penconazole',
       'Pencycuron', 'Picoxystrobin', 'Pirimicarb', 'Prochloraz',
       'Propamocarb', 'Propaquizafop', 'Propyzamide', 'Prosulfocarb',
       'Pymetrozine', 'Pyraclostrobin', 'Pyrimethanil', 'Tebuconazole',
       'Tebufenozide', 'Terbuthylazine', 'Thiacloprid', 'Thiamethoxam',
       'Tribenuron-methyl', 'Trifloxystrobin', 'Tritosulfuron', 'sum']

data[columns_to_convert] = data[columns_to_convert].replace('<LOQ', 0.002)
data[columns_to_convert] = data[columns_to_convert].astype(float)

print(data)

data.dtypes


######################
# Unsupervised learning
######################

## Slecting feature and target variables

data.columns
drop_columns = ['Spinosad', 'nas', 'num','herbicides', 'insecticides', 'fungicides', 'combined_name',
                'stream_segment_single', 'stream_segment_length', 'Total Length','locations','num_buffersize', 
                'Lat/Lon']

data1 = data.drop(columns=drop_columns)
data1.info()

# Subset the DataFrame to include only columns with more than 10 non-null values
data2 = data1.loc[:, data1.notnull().sum() > 10]


## Checking NA values

data2.info()

# Heatmap of missing values
sns.heatmap(data2.isnull(), cbar=True, cmap="viridis")
plt.show()

### Imputing missing values 

def impute_missing_data(data2):
    for col in data2.columns:
        if data2[col].dtype in ['float64']:  
            data2[col] = data2[col].fillna(data2[col].mean()) # use mean
        elif data2[col].dtype == 'object':
            data2[col] = data2[col].fillna(data2[col].mode()[0])  # Use mode
    return data2

data2 = impute_missing_data(data2)

sns.heatmap(data2.isnull(), cbar=False, cmap="viridis")



###########################################################
# Comparing summary statistics before and after imputations
###########################################################

summary_before = data1.describe()
summary_before
summary_after = data2.describe()
summary_after
data2.info()

 
#############################################################################################################
# Distribution of total pesticide concentration across different target variables on the basis of buffersize
#############################################################################################################

data2.columns

# On the basis of stream locations
sns.catplot(data=data2, x='stream', y=np.log1p(data2['sum']),col='buffersize', hue ='stream', kind='box', row='loc')

# On the basis of landcove types
sns.catplot(data=data2, x='Type', y=np.log1p(data2['sum']),col='buffersize', hue="Type", kind='box', col_wrap=3)

# On the basis of croptypes
catplot= sns.catplot(data=data2, x='croptype', y=np.log1p(data2['sum']),col='buffersize', hue="croptype", kind='box', col_wrap=3)
catplot.set_xticklabels(rotation=45, ha='right')


# On the basis of position
catplot= sns.catplot(data=data2, x='position', y=np.log1p(data2['sum']),col='buffersize', hue="position", kind='box', col_wrap=3)
catplot.set_xticklabels(rotation=45, ha='right')



# Grouping by Buffer_Size and Type to calculate mean and std of pesticide concentration
summary_stats = data2.groupby(['buffersize', 'Type'])['sum'].agg(['mean', 'std', 'min', 'max', 'count'])
print("Summary statistics by Buffer_Size and Type:\n", summary_stats)

# Grouping by Buffer_Size and stream to calculate mean and std of pesticide concentration
summary_stats = data2.groupby(['buffersize', 'stream'])['sum'].agg(['mean', 'std', 'min', 'max', 'count'])
print("Summary statistics by Buffer_Size and Stream:\n", summary_stats)

# Grouping by Buffer_Size and stream locations to calculate mean and std of pesticide concentration
summary_stats = data2.groupby(['buffersize', 'loc'])['sum'].agg(['mean', 'std', 'min', 'max', 'count'])
print("Summary statistics by Buffer_Size and Stream locations:\n", summary_stats)

# Grouping by Buffer_Size and sampling positions to calculate mean and std of pesticide concentration
summary_stats = data2.groupby(['buffersize', 'position'])['sum'].agg(['mean', 'std', 'min', 'max', 'count'])
print("Summary statistics by Buffer_Size and Positions:\n", summary_stats)

# Grouping by Buffer_Size and croptypes to calculate mean and std of pesticide concentration
summary_stats = data2.groupby(['buffersize', 'croptype'])['sum'].agg(['mean', 'std', 'min', 'max', 'count'])
print("Summary statistics by Buffer_Size and Croptypes:\n", summary_stats)


data2.info()

# Or

#####################################
# Correlation among all features
#####################################

from scipy.cluster.hierarchy import linkage
from sklearn.decomposition import PCA

# Compute the correlation matrix
corr_data = data2.iloc[:, 1:43]
correlation_matrix = corr_data.corr()

### Filter Correlations Above a Certain Threshold ###
threshold = 0.8
high_corr_pairs = correlation_matrix[(abs(correlation_matrix) > threshold) & (correlation_matrix != 1.0)]

# Melt the DataFrame for better visualization
filtered_corr = high_corr_pairs.stack().reset_index()
filtered_corr.columns = ['Feature1', 'Feature2', 'Correlation']
filtered_corr = filtered_corr.sort_values(by='Correlation', ascending=False)

# Print pairs of features with high correlations
print("Highly Correlated Feature Pairs (correlation > 0.8):")
print(filtered_corr)

### Clustered Heatmap ###
# Perform hierarchical clustering on the correlation matrix
sns.clustermap(correlation_matrix, cmap="coolwarm", annot=False, figsize=(12, 10), method='ward')
plt.title('Clustered Correlation Heatmap')
plt.show()

### Split Features into Groups ###
# Split features into groups of 7
num_features = len(correlation_matrix.columns)
group_size = 7

for i in range(0, num_features, group_size):
    subset = correlation_matrix.iloc[i:i+group_size, i:i+group_size]
    
    # Plot heatmap for the subset
    plt.figure(figsize=(10, 8))
    sns.heatmap(subset, annot=False, cmap="coolwarm")
    plt.title(f'Correlation Heatmap for Features {i+1} to {i+group_size}')
    plt.show()


## Check the data distribution of each column using histogram

corr_data.hist(figsize=(20,20))




######################
# Data reshaping
#####################

import pandas as pd
import numpy as np
from sklearn.preprocessing import RobustScaler
from sklearn.decomposition import KernelPCA
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns


data3 = data2.drop(['sum'], axis=1)
data3.columns
columns_to_logtrs = ['Acetamiprid', 'Azoxystrobin', 'Benalaxyl', 'Bentazone',
       'Boscalid', 'Chlorantraniliprole', 'Chloridazon', 'Clomazone',
       'Clothianidin', 'Cyantraniliprole', 'Cyazofamid', 'Cyflufenamid',
       'Difenconazole', 'Dimethenamid-P', 'Dimethomorph', 'Epoxiconazole',
       'Fenpropimorph', 'Flufenacet', 'Fluopicolid', 'Fluopyram',
       'Imidacloprid', 'Iprovalicarb', 'Isoproturon', 'Mandipropamid',
       'Metalaxyl', 'Metamitron', 'Metobromuron', 'Metolachlor-S',
       'Metrafenone', 'Myclobutanil', 'Napropamide', 'Penconazole',
       'Pirimicarb', 'Propamocarb', 'Propyzamide', 'Prosulfocarb',
       'Pyraclostrobin', 'Tebuconazole', 'Tebufenozide', 'Terbuthylazine',
       'Thiacloprid', 'Trifloxystrobin']

# Log transformation of pesticide columns
for col in columns_to_logtrs:
    data3[col] = np.log1p(data3[col])


# Combine all categorical variables into sample_id
data3['sample_id'] = data3[['stream', 'loc', 'buffersize', 'position', 'Type', 'croptype']].astype(str).agg('_'.join, axis=1)
data3.info()
# Select pesticide concentration columns
pesticide_columns = data3.columns[1:43]


# Prepare data for PCA
numerical_data = data3.iloc[:, 1:43]  # Columns with pesticide concentrations
target_data = data3[['stream', 'loc', 'buffersize', 'position', 'croptype', 'Type', 'sample_id']]       # Categorical columns


robust_scaler = RobustScaler()
numerical_data_scaled = robust_scaler.fit_transform(numerical_data)

# Apply Principal Component Analysis
pca = PCA(n_components=5)
principal_components = pca.fit_transform(numerical_data_scaled)


#######################
# PCA result stored
#######################

# Create a DataFrame with PCA results
pca_df = pd.DataFrame(principal_components, columns=['PC1', 'PC2', 'PC3', 'PC4', 'PC5'])
pca_df = pd.concat([pca_df, target_data.reset_index(drop=True)], axis=1)


##########################
# Explained variance ratio
##########################

# Explained variance ratio and cumulative explained variance
explained_variance_ratio = pca.explained_variance_ratio_
cumulative_explained_variance = np.cumsum(explained_variance_ratio)

# Visualize explained variance
plt.figure(figsize=(10, 6))
plt.bar(range(1, len(explained_variance_ratio) + 1), explained_variance_ratio, alpha=0.7, align='center',
        label='Individual Explained Variance')
plt.step(range(1, len(cumulative_explained_variance) + 1), cumulative_explained_variance, where='mid',
         label='Cumulative Explained Variance', color='red', linestyle='--')
plt.xlabel('Principal Component', fontsize=12)
plt.ylabel('Explained Variance Ratio', fontsize=12)
plt.title('Explained Variance by Principal Components', fontsize=14)
plt.legend(loc='best', fontsize=10)
plt.grid(axis='y', linestyle='--', alpha=0.7)
plt.show()




#########################################################################################
# Overall mean pesticide concentration variations across different categorical variables
#########################################################################################
pca_df.info()
pca_df['loc_buffer'] =  pca_df['loc'].astype(str) + '_' + pca_df['buffersize'].astype(str)
pca_df.info()

#Stream(otter)
otter_stream_data = pca_df[pca_df['stream'] == 'otter']
speyer_stream_data = pca_df[pca_df['stream'] == 'speyer']
queich_stream_data = pca_df[pca_df['stream'] == 'queich']

otter_stream_data.columns


# Define a custom color palette
custom_palette = ['black', '#ff7f0e', '#2ca02c', '#d62728', 'yellow', 'c', '#e377c2', '#7f7f7f', '#bcbd22']

# Register the palette with Seaborn
sns.set_palette(sns.color_palette(custom_palette))

from matplotlib.patches import Ellipse

# Function to draw ellipses with centroids
def biplot_with_centroids_and_ellipses(data, pcs=('PC1', 'PC2'), category='buffersize', **kwargs):
    sns.scatterplot(
        x=pcs[0], 
        y=pcs[1], 
        data=data, 
        hue=category,
        style=category,
        palette=dict(small="black", medium="#ff7f0e", large="#2ca02c"),
        s=100, 
        edgecolor='k',
        alpha=0.8,
        legend=True)
    
    # Generate ellipses for each category
    categories = data[category].unique()
    palette = sns.color_palette(custom_palette, len(categories))  # Changed palette to 'Set2'
    category_colors = dict(zip(categories, palette))
    
    for cat in categories:
        subset = data[data[category] == cat]
        x = subset[pcs[0]]
        y = subset[pcs[1]]
        
        if len(x) < 2:  # Skip if less than 2 points
            continue
        
        # Calculate covariance matrix and mean (centroid)
        cov = np.cov(x, y)
        centroid = np.mean(x), np.mean(y)
        
        # Eigenvalues and eigenvectors
        eigvals, eigvecs = np.linalg.eigh(cov)
        order = eigvals.argsort()[::-1]
        eigvals, eigvecs = eigvals[order], eigvecs[:, order]
        
        # Ellipse properties
        theta = np.degrees(np.arctan2(*eigvecs[:, 0][::-1]))
        width, height = 2 * np.sqrt(eigvals)  # 2-sigma ellipse
        
        # Draw ellipse
        ellipse = Ellipse(
            xy=centroid, 
            width=width, 
            height=height, 
            angle=theta, 
            edgecolor='black', 
            facecolor=category_colors[cat], 
            alpha=0.5,
            linewidth=1.5
        )
        plt.gca().add_patch(ellipse)
        plt.scatter(*centroid, color=category_colors[cat], s=450, zorder=5, edgecolor='black')
        
        

# Function to create faceted stream
def facet_biplot_with_centroids_and_ellipses(pca_df, pcs=('PC1', 'PC2'), category='buffersize', facet='stream'):
    g = sns.FacetGrid(pca_df, col=facet, row='loc', margin_titles=True, height=5)
    g.map_dataframe(biplot_with_centroids_and_ellipses, pcs=pcs, category=category)
    
    # Customize the appearance
    g.set_axis_labels('PC1 (68.77%)', 'PC2 (6.66%)')
    g.set_titles(col_template="{col_name}", size=28)
    g.add_legend(title='Buffersize')
    for ax in g.axes.flat:
        ax.set_xlabel('PC1 (68.77%)', fontsize=28, fontweight='bold')  
        ax.set_ylabel('PC2 (6.66%)', fontsize=28, fontweight='bold')  
        ax.axhline(0, color='grey', linestyle='dotted', linewidth=1.5)  
        ax.axvline(0, color='grey', linestyle='dotted', linewidth=1.5)
        ax.tick_params(axis='x', labelsize=26)  
        ax.tick_params(axis='y', labelsize=26)
        
        
    plt.subplots_adjust(top=0.9)
    g.fig.suptitle("")
    plt.show()

# Call the function with faceting
facet_biplot_with_centroids_and_ellipses(pca_df, pcs=('PC1', 'PC2'), category='buffersize', facet='stream')



# Function to create faceted Type
def facet_biplot_with_centroids_and_ellipses(pca_df, pcs=('PC1', 'PC2'), category='buffersize', facet='Type'):
    g = sns.FacetGrid(pca_df, col=facet, col_wrap=3, margin_titles=True, height=5)
    g.map_dataframe(biplot_with_centroids_and_ellipses, pcs=pcs, category=category)
    
    # Customize the appearance
    g.set_axis_labels('PC1 (68.77%)', 'PC2 (6.66%)')
    g.set_titles(col_template="{col_name}", size=28)
    g.add_legend(title='Buffersize')
    for ax in g.axes.flat:
        ax.set_xlabel('PC1 (68.77%)', fontsize=28, fontweight='bold')  
        ax.set_ylabel('PC2 (6.66%)', fontsize=28, fontweight='bold')  
        ax.axhline(0, color='grey', linestyle='dotted', linewidth=1.5)  
        ax.axvline(0, color='grey', linestyle='dotted', linewidth=1.5)  
        ax.tick_params(axis='x', labelsize=26)  
        ax.tick_params(axis='y', labelsize=26)
    
    plt.subplots_adjust(top=0.9)
    g.fig.suptitle("")
    plt.show()

# Call the function with faceting
facet_biplot_with_centroids_and_ellipses(pca_df, pcs=('PC1', 'PC2'), category='buffersize', facet='Type')





# Filter the DataFrame to exclude multiple crop types
excluded_crops = ['Asparagus', 'Look again', 'Rapeseed?','Carrot', 'Hrbtcl']  
pca_crop = pca_df[~pca_df['croptype'].isin(excluded_crops)]

# Function to create faceted croptypes
def facet_biplot_with_centroids_and_ellipses(pca_crop, pcs=('PC1', 'PC2'), category='buffersize', facet='croptype'):
    g = sns.FacetGrid(pca_crop, col=facet, col_wrap=3, margin_titles=True, height=7)
    g.map_dataframe(biplot_with_centroids_and_ellipses, pcs=pcs, category=category)
    
    # Customize the appearance
    g.set_axis_labels('PC1 (68.77%)', 'PC2 (6.66%)')
    g.set_titles(col_template="{col_name}", size=28)
    g.add_legend(title='Buffersize')
    for ax in g.axes.flat:
        ax.set_xlabel('PC1 (68.77%)', fontsize=28, fontweight='bold')  
        ax.set_ylabel('PC2 (6.66%)', fontsize=28, fontweight='bold')  
        ax.axhline(0, color='grey', linestyle='dotted', linewidth=1.5)  
        ax.axvline(0, color='grey', linestyle='dotted', linewidth=1.5)  
        ax.tick_params(axis='x', labelsize=26)  
        ax.tick_params(axis='y', labelsize=26)
    
    plt.subplots_adjust(top=0.9)
    g.fig.suptitle("")
    plt.show()

# Call the function with faceting
facet_biplot_with_centroids_and_ellipses(pca_crop, pcs=('PC1', 'PC2'), category='buffersize', facet='croptype')


# Function to create faceted croptypes
def facet_biplot_with_centroids_and_ellipses(pca_df, pcs=('PC1', 'PC2'), category='buffersize', facet='position'):
    g = sns.FacetGrid(pca_df, col=facet, col_wrap=3, margin_titles=True, height=7)
    g.map_dataframe(biplot_with_centroids_and_ellipses, pcs=pcs, category=category)
    
    # Customize the appearance
    g.set_axis_labels('PC1 (68.77%)', 'PC2 (6.66%)')
    g.set_titles(col_template="{col_name}", size=28)
    g.add_legend(title='Buffersize', fontsize=24)
    for ax in g.axes.flat:
        ax.set_xlabel('PC1 (68.77%)', fontsize=28, fontweight='bold')  
        ax.set_ylabel('PC2 (6.66%)', fontsize=28, fontweight='bold')  
        ax.axhline(0, color='grey', linestyle='dotted', linewidth=1.5)  
        ax.axvline(0, color='grey', linestyle='dotted', linewidth=1.5)  
        ax.tick_params(axis='x', labelsize=26)  
        ax.tick_params(axis='y', labelsize=26)
        
    plt.subplots_adjust(top=0.9)
    g.fig.suptitle("")
    plt.show()

# Call the function with faceting
facet_biplot_with_centroids_and_ellipses(pca_df, pcs=('PC1', 'PC2'), category='buffersize', facet='position')



##############
# PCA Loadings
##############

# Compute the loadings (PCA components)
loadings = pca.components_.T

# Convert loadings to DataFrame for easier interpretation
loadings_df = pd.DataFrame(loadings, columns=[f"PC{i+1}" for i in range(pca.n_components)],
                            index=numerical_data.columns)

# Select the top 3 principal components for plotting
pcs_to_plot = ['PC1', 'PC2', 'PC3']
loadings_subset = loadings_df[pcs_to_plot]

# Melt the DataFrame for easier plotting with seaborn
loadings_melted = loadings_subset.reset_index().melt(id_vars='index', var_name='Principal Component', value_name='Loading')


# Filter rows with Loading > 0.2 or < -0.2
filtered_loadings = loadings_melted[(loadings_melted['Loading'] > 0.4) | (loadings_melted['Loading'] < -0.4)]


# Create a barplot
plt.figure(figsize=(15, 8))
sns.barplot(
    data=filtered_loadings,
    x='index', y='Loading', hue='Principal Component',
    palette='Set2'
)

# Rotate x-axis labels for better visibility
plt.yticks(fontsize=14)
plt.xticks(rotation=90, fontsize=14)
plt.xlabel('Chemical Names', fontsize=18, fontweight='bold')
plt.ylabel('Loading', fontsize=18, fontweight='bold')
plt.title('Loadings for PC1, PC2, and PC3', fontsize=16)
plt.legend(title='Principal Component', fontsize=12, title_fontsize=14)
plt.tight_layout()

plt.show()


# Identify significant chemicals from PC loadings
significant_chemicals = ['Metolachlor-S', 'Cyazofamid', 'Mandipropamid', 'Tebuconazole']

data3.columns
# Subset data for selected chemicals and categorical variables
subset_data = data3[significant_chemicals + ['Type', 'stream', 'loc', 'buffersize','position', 'croptype']]
subset_data['combined_name'] = subset_data[['stream', 'loc', 'buffersize']].astype(str).agg('_'.join, axis=1)
subset_data.info()



############
#ANOVA
############


from scipy.stats import f_oneway, kruskal

results = {}
significant_results = []

# Calculating overall significance
for chem in significant_chemicals:
    results[chem] = {}
    for cat in ['Type', 'combined_name', 'position', 'croptype']:
        groups = [subset_data[chem][subset_data[cat] == level] for level in subset_data[cat].unique()]
        
        # Perform ANOVA
        if all(len(group) > 1 for group in groups):  
            f_stat, p_value = f_oneway(*groups)
            results[chem][cat] = {'F-stat': f_stat, 'p-value': p_value}
            
            # Check for significance
            if p_value < 0.05:  # Threshold for significance
                significant_results.append({
                    'Chemical': chem,
                    'Category': cat,
                    'F-stat': f_stat,
                    'p-value': p_value
                })
        else:
            results[chem][cat] = {'F-stat': None, 'p-value': None}


significant_df = pd.DataFrame(significant_results)
print(significant_df)


############################################################
# Groupwise comparison for significant categorical variables
###########################################################

from statsmodels.stats.multicomp import pairwise_tukeyhsd
from statsmodels.stats.multitest import multipletests

posthoc_results = []

# Loop through significant combinations
for _, row in significant_df.iterrows():
    chem = row['Chemical']
    cat = row['Category']
    
    if cat in subset_data.columns:
        data = subset_data[[chem, cat]].dropna()
        
        # Perform Tukey's HSD test
        tukey = pairwise_tukeyhsd(
            endog=data[chem],
            groups=data[cat],  
            alpha=0.05
        )
        
        # Collect results for all pairwise comparisons
        for result in tukey.summary().data[1:]:  # Skip the header row
            posthoc_results.append({
                'Chemical': chem,
                'Category': cat,
                'Group 1': result[0],
                'Group 2': result[1],
                'Diff': result[2],
                'Lower CI': result[3],
                'Upper CI': result[4],
                'p-value': result[5],
                'Reject Null': result[6]
            })

posthoc_df = pd.DataFrame(posthoc_results)
posthoc_df['Adj p-value'] = multipletests(posthoc_df['p-value'], method='bonferroni')[1]

# Filter significant results
significant_posthoc_df = posthoc_df[(posthoc_df['p-value'] < 0.05) & (posthoc_df['Adj p-value'] < 0.05)]

# Final post-hoc results dataframe
print(significant_posthoc_df)







# Visualization of Metolachlor-S concentration variation across significant type and position factors

# Boxplot for Landcovertypes
plt.figure(figsize=(10, 6))
sns.boxplot(data=subset_data, x='Type', y='Metolachlor-S', palette='Set1')
plt.title('', fontsize=16)
plt.xlabel('Landcover Types', fontsize=18, fontweight='bold')
plt.ylabel('Metolachlor-S conc.', fontsize=18, fontweight='bold')
plt.xticks(fontsize=16)
plt.yticks(fontsize=16)
plt.tight_layout()
plt.show()


# Boxplot for sample position
plt.figure(figsize=(10, 6))
sns.boxplot(data=subset_data, x='position', y='Metolachlor-S', palette='Set1')
plt.title('', fontsize=16)
plt.xlabel('Sample position', fontsize=18, fontweight='bold')
plt.ylabel('Metolachlor-S conc.', fontsize=18, fontweight='bold')
plt.xticks(fontsize=16)
plt.yticks(fontsize=16)
plt.tight_layout()
plt.show()






# Visualization of Tebuconazole  concentration variation across significant type, and position factors

# Boxplot for Landcovertypes
plt.figure(figsize=(10, 6))
sns.boxplot(data=subset_data, x='Type', y='Tebuconazole', palette='Set1')
plt.title('', fontsize=16)
plt.xlabel('Landcover Types', fontsize=18, fontweight='bold')
plt.ylabel('Tebuconazole conc.', fontsize=18, fontweight='bold')
plt.xticks(fontsize=16)
plt.yticks(fontsize=16)
plt.tight_layout()
plt.show()


# Boxplot for position
plt.figure(figsize=(10, 6))
sns.boxplot(data=subset_data, x='position', y='Tebuconazole', palette='Set1')
plt.title('', fontsize=16)
plt.xlabel('Sample position', fontsize=18, fontweight='bold')
plt.ylabel('Tebuconazole conc.', fontsize=18, fontweight='bold')
plt.xticks(rotation=0,fontsize=16)
plt.yticks(fontsize=16)
plt.tight_layout()
plt.show()






# Visualization of Mandipropamid  concentration variation across significant type, croptype, position, combined factors

# Boxplot for Landcovertypes
plt.figure(figsize=(10, 6))
sns.boxplot(data=subset_data, x='Type', y='Mandipropamid', palette='Set1')
plt.title('', fontsize=16)
plt.xlabel('Landcover Types', fontsize=18, fontweight='bold')
plt.ylabel('Mandipropamid conc.', fontsize=18, fontweight='bold')
plt.xticks(fontsize=16)
plt.yticks(fontsize=16)
plt.tight_layout()
plt.show()

# Boxplot for croptypes
plt.figure(figsize=(10, 6))
sns.boxplot(data=subset_data, x='croptype', y='Mandipropamid', palette='Set1')
plt.title('', fontsize=16)
plt.xlabel('Croptypes', fontsize=18, fontweight='bold')
plt.ylabel('Mandipropamid conc.', fontsize=18, fontweight='bold')
plt.xticks(rotation=45,fontsize=16)
plt.yticks(fontsize=16)
plt.tight_layout()
plt.show()

# Boxplot for position
plt.figure(figsize=(10, 6))
sns.boxplot(data=subset_data, x='position', y='Mandipropamid', palette='Set1')
plt.title('', fontsize=16)
plt.xlabel('Sample position', fontsize=18, fontweight='bold')
plt.ylabel('Mandipropamid conc.', fontsize=18, fontweight='bold')
plt.xticks(rotation=0,fontsize=16)
plt.yticks(fontsize=16)
plt.tight_layout()
plt.show()

# Boxplot for combined name
plt.figure(figsize=(10, 6))
sns.boxplot(data=subset_data, x='combined_name', y='Mandipropamid', hue="stream")
plt.title('', fontsize=16)
plt.xlabel('Interactions of stream location and buffersize', fontsize=18, fontweight='bold')
plt.ylabel('Mandipropamid conc.', fontsize=18, fontweight='bold')
plt.xticks(rotation=90,fontsize=12)
plt.yticks(fontsize=12)
plt.tight_layout()
plt.show()












