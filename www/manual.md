# StaMPS-Visualizer 3.0 Manual
1. [Export custom data from StaMPS](#export)
	1. [Troubleshooting](#trsht1)
2. [Import custom StaMPS data](#import)
3. [Prepare data for visualisation](#prep)
4. [Export custom data from SNAP (baseline plot)](#exportbl)
5. [Import custom baseline plot data](#importbl)
6. [Export results from the Visualizer](#exportres)

## Export custom data from StaMPS <a name="export"></a>

* process [StaMPS](https://homepages.see.leeds.ac.uk/~earahoo/stamps/StaMPS_Manual_v4.1b1.pdf) until step 6 or further
* use the following lines in Matlab to export the data. Run the code line by line and follow the instructions in the comments:

For **StaMPS 4.x**:

```
% the 'v-doa' parameter is an example you can change it to your needs
ps_plot('v-doa', 'ts'); 

% a new window will open
% in the new window select a radius and location of the radius center to select the PS to export

load parms.mat;

% the 'v-doa' parameter is an example you can change it to your needs
% but be sure that you use the same paramters as above in the ps_plot()!
ps_plot('v-doa', -1);

load ps_plot_v-doa.mat;
lon2_str = cellstr(num2str(lon2));
lat2_str = cellstr(num2str(lat2));
lonlat2_str = strcat(lon2_str, lat2_str);

lonlat_str = strcat(cellstr(num2str(lonlat(:,1))), cellstr(num2str(lonlat(:,2))));
ind = ismember(lonlat_str, lonlat2_str);

disp = ph_disp(ind);
disp_ts = ph_mm(ind,:);
export_res = [lon2 lat2 disp disp_ts];

metarow = [ref_centre_lonlat NaN transpose(day)-1];
k = 0;
export_res = [export_res(1:k,:); metarow; export_res(k+1:end,:)];
export_res = table(export_res);

% you can specify the location and name of the .csv export by renaming the second parameter
writetable(export_res,'stamps_tsexport.csv')
```

If you use **StaMPS 3.x**, use this script:

```
% the 'v-doa' parameter is an example you can change it to your needs
ps_plot('v-doa', 'ts'); 

% a new window will open
% in the new window select a radius and location of the radius center to select the PS to export

load parms.mat;

% the 'v-doa' parameter is an example you can change it to your needs
% but be sure that you use the same paramters as above in the ps_plot()!
ps_plot('v-doa', -1);

load ps_plot_v-doa.mat;
lon2_str = cellstr(num2str(lon2));
lat2_str = cellstr(num2str(lat2));
lonlat2_str = strcat(lon2_str, lat2_str);

lonlat_str = strcat(cellstr(num2str(lonlat(:,1))), cellstr(num2str(lonlat(:,2))));
ind = ismember(lonlat_str, lonlat2_str);

disp = ph_disp(ind);
export_res = [lon2 lat2 disp ts];

metarow = [ref_centre_lonlat NaN transpose(day)-1];
k = 0;
export_res = [export_res(1:k,:); metarow; export_res(k+1:end,:)];
export_res = table(export_res);

% you can specify the location and name of the .csv export by renaming the second parameter
writetable(export_res,'stamps_tsexport.csv')
```

### Troubleshooting <a name="trsht1"></a>

In case you encounter this error during the export process:

```
Error using horzcat
Dimensions of arrays being concatenated are not consistent.
```

* When you select your radius during the ```ps_plot('v-doa', 'ts')``` step, choose a very very large radius like 1000000 which will securely include all of your PS.
* Now import the exported ```.csv``` table to the Visualizer open the app and go to the **Prepare and Export** tab and perform a spatial subset, see [Prepare data for visualisation](#prep)


## Import custom StaMPS data <a name="import"></a>

After the export from Matlab/StaMPS was successful, the ```.csv``` table can be imported to the Visualizer:

* move the ```.csv``` file to th ```StaMPS_Visualizer/stusi``` directory
* open or restart the app
* now the new study site can be selected in the Visualizer control panel

## Prepare data for visualisation <a name="prep"></a>


## Export custom data from SNAP (baseline plot) <a name="exportbl"></a>

## Import custom baseline plot data <a name="importbl"></a>

## Export results from the Visualizer <a name="exportres"></a>
