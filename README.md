# pop_projections_shiny_tool
A shiny tool to display the Small Area Population Projections produced by the Improvement Service.

# Updating the tool
If new data is added the script Add-aggregations-to-data.R needs to be run first. This script adds aggregations to the projection data and saves as a csv called which is then read into the global script. This initial script can be found in Resources - Helper files

# Resources

A mockup of the app has been created in Mockflow. A PDF copy of this can be viewed in the resources folder. 

The following flowchart has been created to demonstrate how different parts of the code are related and the process that will be taken for creating the map
https://impservihub-my.sharepoint.com/:u:/g/personal/cara_connachan_improvementservice_org_uk/EUdY7VaZNmpJn0vg7LGxw5UBGq5x0hbnHyZIbvGn9UU2vw?e=PRcxBd

The app will follow a similar structure to the COVID Economic Impact app and so may use similar code. The code is saved here 
https://github.com/Improvement-Service/Covid-Economic-Impact

Code in this tool will follow the Tidyverse style guide which can be viewed here
https://style.tidyverse.org/index.html

# Working Together
At the beginning of the project the tasks for the global file have already been complete so that the data needed elsewhere is ready to use. Some packages will still need to be loaded in the global. 

The UI and Server files have comments added to them mapping out the tasks that need to be complete and where the code should be entered. 

For each chunk of code an Issue has been opened detailing what needs to be done. These issues were created in the order they should be completed, to view them in order sort from oldest to newest. (The issues for tab 1 and tab 2 are independent of each other so can be carried out at the same time if need be). Each issue has been tagged to show what and where it relates to. Specific types of code (maps, graphs, bespoke functions and reactive expressions) have been tagged so that collaborators can work on the type of issues they are comfortable with. 

To start work on an issue:
1. Select the name of an issue
2. At the right hand-side where it says "Assignees" assign yourself to the issue
3. At the right hand-side where it says "Development" create a branch for the issue
4. Once you have worked on your code open a pull request for the branch and associate it with the issue
5. Once the pull request has been reviewed and agreed the branch can be merged with the Master branch
6. Finally close the issue and pull request and delete the branch
