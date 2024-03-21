# About the Project

[CellMinerCDB](https://discover.nci.nih.gov/rsconnect/cellminercdb/) provides a web-based resource for integrating multiple forms of pharmacological and genomic analyses, and unifying the richest cancer cell line datasets (the NCI-60, Sanger/MGH GDSC, and Broad CCLE/CTRP).

This project will be aiming towards an additional module in this project for visulization of averaged genetic profiling data on top of some curated set of pathways.The project is inspired by [PathwayMapper](https://www.pathwaymapper.org/#).

Final Link to the code
[Link](https://github.com/sidd-2203/cellminercdb/tree/GSoC-Staging)

This project will be aiming towards an additional module in this project for the visualization of averaged genetic profiling data on top of some curated set of pathways. The project is inspired by [PathwayMapper](https://www.pathwaymapper.org/#).

# New Pathway Addition

To add a new pathway to the set of existing pathways one has to follow the steps mentioned below:

- Draw the pathway on [PathwayMapper](https://www.pathwaymapper.org/#).
- Export the pathway as a text file.
- Add the file to the folder named pathway_mapper_networks in the Cellminer CDB project folder.
- Execute the script createPathwayData.R present in the project folder. This would load the new pathway to the pathway_as_json.json file which is used to load all the pathways.
- Run the project.
