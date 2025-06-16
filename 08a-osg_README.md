# Obtaining VSOA with Bootstrapped Confidence Intervals

Bootstrapping the confidence interval for each VSOA estimate is computationally expensive, particularly when performing enough iterations to support Bonferroni correction.

This research was done using services provided by the [OSG Consortium](https://osg-htc.org) [1,2,3,4], which is supported by the National Science Foundation awards #2030508 and #2323298. They provide many resources to facilitate using their services.

You will need to [create a custom container with R and the `boot` package](https://portal.osg-htc.org/documentation/software_examples/r/tutorial-R-addlibSNA/#create-a-custom-container-with-r-packages). We used the following script named `R4_0_2-boot.def`.

```
# R4_0_2-boot.def
Bootstrap: docker
From: opensciencegrid/osgvo-r:4.0.2

%post
    R -e 'install.packages("boot", dependencies=TRUE, repos="https://mirrors.nics.utk.edu/cran/")'

```

With the following command:

```
apptainer build R4_0_2-boot.sif R4_0_2-boot.def

```

You should ensure that the container is staged appropriately so it is transfered using the correct infrastructure. See [Transfer Larger Job Files and Containers using OSDF](https://portal.osg-htc.org/documentation/htc_workloads/managing_data/osdf/) for details.


Depending on what you named the container and where you have stored it, the first line of `08b-osg_bootstrap.sub` should be something like:

```
+SingularityImage = "osdf:///ospool/ap20/data/{username}/containers/R4_0_2-boot.def"
```

The `.sub` file is the script that communicates with HTCondor, the system that coordinates the job queue and where jobs are executed and where data is returned (among other things) via the [`condor_submit`](https://htcondor.readthedocs.io/en/latest/man-pages/condor_submit.html) command. This sub file references the `item-id-label.csv` file for defining jobs. Once a job (which will compute the VSOA for a single word) is paired with an execute node and the necessary data is transferred to that node, the `08c-osg_bootstrap.sh` script will be executed on the code, which in turn will run the `08d-osg_bootstrap.R` script within the R environment containerized above.


## Expected output
There should be a directory structure of results constructed in the directory where you ran `condor_submit 08b-osg_bootstrap.sub`.

```
results/
  ci_bonf/
    glm/
       {a file for each num_item_id in item-id-label.csv}
    bs_ci/
       {a file for each num_item_id in item-id-label.csv}
    bs/
       {a file for each num_item_id in item-id-label.csv}

```


## If things go wrong
Jobs can fail for many reasons. You can reattempt VSOA estimates for specific words by creating a copy of `item-id-label.csv` and reducing it to only the words that did not complete correctly the first time. These jobs will be assigned a different Cluster ID, and so have a different file prefix.

If jobs failed because they ran out of memory (i.e., you get an error like "memory usage exceeds requested amount"), then bump up the memory request in the submit file before resubmitting.

If jobs failed because files failed to transfer (while data transfer worked fine for other jobs in the same cluster), or you get a "Shadow Error", were "evicted", or generally any error that does not specifically indicate the problem, then simply rerunning theose jobs may resolve the issue, without you needing to change anything.

If all your jobs fail, then there is probably an incorrect filename somewhere in the scripts. Or you may need to create `log` and `results` folders for job output to be written to.


## Retrieving data
Transfering many small files can be slow. `tar cf results.tar results/` will archive the ~2000 result files into a single file that can be downloaded more efficiently. The whole results directory tree should be extracted into your project directly. The script `09-prep-data-for-analysis.R` will look for files in this directory structure.


## Conclusion
OSG is a wonderful resource and a powerful tool, but there is a learning curve. This README and associated OSG scripts are provided without warrantee, and will not be updated as OSG revises their protocols or infrastructure. See their reference material for up to date instructions and tutorials, in case the scripts break. They are provided as is, and I cannot provided technical support if they do not work for you out of the box.

CRC


1. Pordes, R., Petravick, D., Kramer, B., Olson, D., Livny, M., Roy, A., Avery, P., Blackburn, K., Wenaus, T., Würthwein, F., Foster, I., Gardner, R., Wilde, M., Blatecky, A., McGee, J., & Quick, R. (2007). The open science grid. J. Phys. Conf. Ser., 78, 012057. https://doi.org/10.1088/1742-6596/78/1/012057

2. Sfiligoi, I., Bradley, D. C., Holzman, B., Mhashilkar, P., Padhi, S., & Wurthwein, F. (2009). The pilot way to grid resources using glideinWMS. 2009 WRI World Congress on Computer Science and Information Engineering, 2, 428–432. https://doi.org/10.1109/CSIE.2009.950

3. OSG. (2006). OSPool. OSG. https://doi.org/10.21231/906P-4D78

4. OSG. (2015). Open Science Data Federation. OSG. https://doi.org/10.21231/0KVZ-VE57
