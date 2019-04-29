# Climate Data Tools

Climate Data Tools `CDT` is a  set of utility functions for meteorological data quality control, homogenization and merging station data with satellite and others proxies such as reanalysis, all functions are available in the GUI mode.

## `CDT` installation on Windows

### 1) Install `R`

Go to the CRAN website [CRAN](https://CRAN.R-project.org).
Then click on the link [Download R for Windows](https://cran.r-project.org/bin/windows/) and [base](https://cran.r-project.org/bin/windows/base/). Download the latest version of `R`. For example: [Download R 3.6.0 for Windows](https://cran.r-project.org/bin/windows/base/R-3.6.0-win.exe) (62 megabytes, 32/64 bit).
Install the downloaded file (example: R-3.6.0-win.exe). Perform a default installation (Just click on **Next**).

### 2) Install `CDT`

Open `R` and install `devtools` package with:

```
install.packages("devtools")
```
Now, you can install the the development version of `CDT` from GitHub with:

``` r
library(devtools)
install_github("rijaf-iri/CDT")
```

## `CDT` installation on MacOS X

### 1) Install `XQuartz`
Type `XQuartz` in Apple's search, if you don't find it, it means, `XQuartz` is not already installed. It's better to update `XQuartz` to the latest version if it is already installed.
Download and install it from [https://www.xquartz.org](https://www.xquartz.org/).
Restart your computer

### 2) Install `Tcl/Tk`

#### Check `Tcl/Tk` installation
Check if the `Tcl/Tk` libraries are already installed in your computer. Open a terminal and run the following command:

```bash
echo 'puts $tcl_library;exit 0' | tclsh
```
If `Tcl/Tk` libraries are installed in your computer, you should get the path to the Tcl library like this: `/usr/local/lib/tcl8.6`.

If `Tcl/Tk` libraries are not yet installed in your computer, you can install ActiveTcl&#174;. Go directly to the ActiveTcl&#174; installation below.

#### Check `Tktable` package
Check if the package `Tktable` is installed. Run the following command from terminal:

```bash
echo 'puts [package require Tktable];exit 0' | tclsh
```
If the package is installed, you should have the version number of the package. If `Tktable` is not installed, you can download it from [here](https://sourceforge.net/projects/tktable/files/tktable/2.10/Tktable2.10.tar.gz/download).

Change to the directory where you downloaded `Tktable` archive file.

```bash
cd ~/path/to/Tktable/archive
```
Uncompressed the archive file.

```bash
tar -zxvf Tktable2.10.tar.gz
```
This will create a subdirectory `Tktable2.10` with all the files in it. Change to this directory.

```bash
cd Tktable2.10
```

The default installation path of `Tktable` is under `/usr/local`. If the Tcl Library Path directory is under this directory, you can install `Tktable` by default using the following commands:

```bash
./configure
make install
```

If Tcl Library Path directory is anywhere else. Check the `auto_path` global variable using the following command

```bash
echo 'puts $auto_path;exit 0' | tclsh
```

You should have a list of directories like this: `/usr/local/lib/tcl8.6` `/usr/local/lib`. You need to set the `--prefix` option of configure

```bash
./configure --prefix=/usr/local
make install
```

#### Check `BWidget` package
Check if the package BWidget is installed. Run the following command from terminal:

```bash
echo 'puts [package require BWidget];exit 0' | tclsh
```

If the package is installed, you should have the version number of the package. If `BWidget` is not installed, you can download it from [here](https://sourceforge.net/projects/tcllib/files/BWidget/1.9.12/bwidget-1.9.12.zip/download).
Unzipped the file `bwidget-1.9.12.zip` and copy it under the Tcl Library Path directory. Copy the bwidget directory `bwidget-1.9.12` under one of the `auto_path` global variable directories.

>  **Note**
>  If you install `Tktable` and `BWidget` anywhere else, remember the path you put the packages, you will need it when you install `CDT`.

#### Install ActiveTcl&#174;

Download ActiveTcl&#174; from [https://www.activestate.com/products/activetcl/downloads/](https://www.activestate.com/products/activetcl/downloads/).

ActiveTcl&#174; executables will be installed (`wish`, `tclsh` and `tkcon`) in `/usr/local/bin` and the library will be put in `/usr/local/lib/tcl8.6` or `/usr/local/lib/tcl8.5` depending on the version.

`Tktable` will be put in `/Library/Tcl/teapot/package/macosx10.5-i386-x86_64/lib/Tktable2.11` 
and `BWidget` in `/Library/Tcl/teapot/package/tcl/lib/BWidget1.9.8`. 

### 3) Install GDAL/GEOS/PROJ.4
Download and install `GDAL` binaries from [http://www.kyngchaos.com/software/frameworks](http://www.kyngchaos.com/software/frameworks). Install the latest version of **GDAL Complete**.

`GDAL` will be installed in `/Library/Frameworks/GDAL.framework`
`GEOS` in `/Library/Frameworks/GEOS.framework`
and `PROJ` in `/Library/Frameworks/PROJ.framework`

The configuration files are located in
`GEOS`:  `/Library/Frameworks/GEOS.framework/unix/bin/geos-config`
`GDAL`:  `/Library/Frameworks/GDAL.framework/unix/bin/gdal-config`

And `PROJ` `include` and `lib` are located in `/Library/Frameworks/PROJ.framework/unix/include` and
 `/Library/Frameworks/PROJ.framework/unix/lib` respectively.
Remember these paths, you will need them when you install the package `rgdal` and `rgeos` on `R`.

### 4) Install `R`
Download and install `R` binary for your MacOS X version from [https://cran.r-project.org/bin/macosx](https://cran.r-project.org/bin/macosx/)

### 5) Install `rgeos` and `rgdal`

Open `R` and install `rgdal` package with:

```r
install.packages("rgdal", type = "source", 
configure.args = c(
   "--with-proj-include=/Library/Frameworks/PROJ.framework/unix/include", 
   "--with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib", 
   "--with-gdal-config=/Library/Frameworks/GDAL.framework/unix/bin/gdal-config"
  )
)
```

Install `rgeos` package with:

```r
install.packages("rgeos", type = "source", 
configure.args = 
   "--with-geos-config=/Library/Frameworks/GEOS.framework/unix/bin/geos-config"
)
```

### 6) Install `CDT`
Open `R` and install `devtools` package with: 

```r
install.packages("devtools")
```

Now, you can install the development version of `CDT` from GitHub with:

```r
library(devtools)
install_github("rijaf-iri/CDT")
```

If you get a warning message telling you that `Tktable` or `BWidget` not found, you need to edit CDT's local configuration for Tcl: `~/Library/Application Support/CDT/config/Tcl_config.json`.
Go to the MacOS configuration and change `Tktable.auto` or `Bwidget.auto` to `false`, then set the full path to `Tktable` or `BWidget` directory with `Tktable.path` or `Bwidget.path`, as shown in the following example:

```json
"MacOS": {
    "Tktable.auto": false,
    "Tktable.path": "/Library/Tcl/teapot/package/macosx10.5-i386-x86_64/lib/Tktable2.11",
    "Bwidget.auto": false,
    "Bwidget.path": "/Library/Tcl/teapot/package/tcl/lib/BWidget1.9.8"
  },
```

After editing `Tcl_config.json`, save it. Open a new `R` session and load and start `CDT`.


## `CDT` installation on Ubuntu

### 1) Install `Tcl/Tk`

Check if the `Tcl/Tk` libraries are already installed in your computer, make sure that the `*-dev` packages are installed. If not, you can install it with:

```bash
sudo apt-get install tk-dev tcl-dev
```

Check if the Tcl package `Tktable` is installed. If it is not installed, you can download it from [here](https://sourceforge.net/projects/tktable/files/tktable/2.10/Tktable2.10.tar.gz/download). 

Check if the Tcl package `BWidget` is installed. If it is not installed, you can download it from [here](https://sourceforge.net/projects/tcllib/files/BWidget/1.9.12/bwidget-1.9.12.zip/download).

>  **Note**
> See MacOS X `Tktable` and `BWidget` installation.

### 2) Install GDAL/OGR

Add the `PPA` to your sources

```bash
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt-get update && sudo apt-get upgrade
```

Install `GDAL`

```bash
sudo apt-get install gdal-bin libgdal-dev
```

Verify the installation with

```bash
ogrinfo
```

Get the installation path of `gdal-config` and `geos-config`. Save these paths somewhere, you will need it later when installing the `R` packages `rgdal` and `rgeos`.


### 3) Install `NetCDF`

```bash
sudo apt-get install netcdf-bin libnetcdf-dev
```

### 4) Install `R`

Install `R` if not installed yet.
Add the `R` repository to your sources

```bash
sudo add-apt-repository "deb http://cran.rstudio.com/bin/linux/ubuntu $(lsb_release -sc)/"
sudo apt-get update
```

Add GPG key

```bash
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
```

Install R

```bash
sudo apt-get install r-base r-base-dev
```

### 5) Install `R` packages `ncdf4` and `tkrplot`

```bash
sudo apt-get install r-cran-tkrplot r-cran-ncdf4
```

You can install `ncdf4` package with:

```r
## edit the path
nc_config <- '/usr/bin/nc-config'

install.packages('ncdf4', type = "source",
        configure.args = paste0('--with-nc-config=', nc_config))
```

### 6) Install `rgeos` and `rgdal`

Open `R` and install `rgdal` package with:

```r
## edit the paths
gdal_config <- '/usr/bin/gdal-config'
proj_include <- '/usr/include'
proj_lib <- '/usr/lib'

install.packages("rgdal", type = "source", 
         configure.args = c(
                       paste0('--with-proj-include=', proj_include),
                        paste0('--with-proj-lib=', proj_lib),
                        paste0('--with-gdal-config=', gdal_config)
                      )
                  )
```

Install `rgeos` package with:

```r
## edit the path
geos_config <- '/usr/bin/geos-config'

install.packages("rgeos", type = "source", 
       configure.args = paste0('--with-geos-config=', geos_config)
       )
```

### 7) Install CDT

Open `R` and install `devtools` package with: 

```r
install.packages("devtools")
```
Now, you can install the development version of `CDT` from GitHub with:

```r
devtools::install_github("rijaf-iri/CDT")
```

If you get a warning message telling you that `Tktable` or `BWidget` not found, you need to edit CDT’s local configuration for Tcl:  `~/.local/CDT/config/Tcl_config.json`
Go to the Linux configuration and change `Tktable.auto` or `Bwidget.auto` to `false`, then set the full path to `Tktable` or `BWidget` directory with `Tktable.path` or `Bwidget.path`.

## Usage

``` r
# Load  CDT library
library(CDT)

# Starting CDT
startCDT()
```

## Updating `CDT`

To only update `CDT` without updating all dependencies packages, enter the following command on `R` console

```r
devtools::install_github("rijaf-iri/CDT", dependencies = FALSE, upgrade_dependencies = FALSE, force = TRUE)
```

To update `CDT` and all dependencies packages, use 

```r
devtools::install_github("rijaf-iri/CDT")
```
