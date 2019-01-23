# `shipshape`

## Intuitive way to conduct shape analysis.

The package consists of two main functions: `shape_proc_distance` for calculating Procrustes distance metrics, and `shape_elastic_distance` for calculating Elastic distance metrics.
 
---------------

### Installation

To install the latest version directly from Github, please use:
<pre><code>install.packages("devtools")
devtools::install_github("goutham1220/shipshape")
</code></pre>

### Usage

In order to use the `shape_proc_distance` and `shape_elastic_distance` functions, your data should be contained in a 3-dimensional array. 
Each object should occupy a separate page of the array. With this in mind, if your data was contained in a 3D array `arr`, the functions can be called like this:
<pre><code>shape_proc_distance(arr, type = "full")</code></pre>
<pre><code>shape_elastic_distance(arr, mode = "C")</code></pre>

The functions will then calculate all nC2 combinations of distances and return the distances, properly labeled, as a vector, like so:

<pre><code> 1 2       1 3       2 3 
0.1974612 0.3008154 0.3186260</code></pre>
