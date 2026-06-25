This is a repo for a package to visualize XTRACT data similar to ggseg. This is a work in progress, please use with caution!

xtractview::plot_xtract(my_tract_data)+theme_abyss()


## Data Format Requirements

The plotting functions in `xtractview` expect a standard `data.frame` or tibble structured with a minimum of two columns:

1. **`region`**: Case-sensitive character strings matching supported XTRACT tract names exactly (e.g., `"Corticospinal Tract L"`).
2. **Metric Column**: A numeric column containing data values bounded between `0.0` and `1.0` (such as Fractional Anisotropy). The function will dynamically map the first non-region column it encounters.

| region | Mean_FA |
| :--- | :--- |
| Corticospinal Tract L | 0.55 |
| Arcuate Fasciculus R | 0.62 |
| Forceps Minor | 0.41 |

<img width="1629" height="1083" alt="image" src="https://github.com/user-attachments/assets/52f3cc39-ee51-49ef-86fe-a834d221e9f0" />
