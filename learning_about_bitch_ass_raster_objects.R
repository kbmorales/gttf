rdl <- brick("products/figures/redlinemap.tif")
redline <- raster("products/figures/redlinemap.tif")
rdl2 <- stack("products/figures/redlinemap.tif")
# finding out shit about these files
# these are set up like sp objects
# they have multiple data layers
rdl # for multilayer raster objects
# class       : RasterBrick 
# dimensions  : 3103, 3063, 9504489, 3  (nrow, ncol, ncell, nlayers)
# resolution  : 1.624475e-06, 1.624475e-06  (x, y)
# extent      : 39.30517, 39.31015, -76.64618, -76.64114  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs 
# data source : /cloud/project/products/figures/redlinemap.tif 
# names       : redlinemap.1, redlinemap.2, redlinemap.3 
# min values  :            0,            0,            0 
# max values  :          255,          255,          255 



redline
# class       : RasterLayer 
# band        : 1  (of  3  bands)
# dimensions  : 3103, 3063, 9504489  (nrow, ncol, ncell)
# resolution  : 1.624475e-06, 1.624475e-06  (x, y)
# extent      : 39.30517, 39.31015, -76.64618, -76.64114  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs 
# data source : /cloud/project/products/figures/redlinemap.tif 
# names       : redlinemap 
# values      : 0, 255  (min, max)


# let's look at the redline object first

redline@file
# An object of class ".RasterFile"
# Slot "name":
#   [1] "/cloud/project/products/figures/redlinemap.tif"
# Slot "datanotation":
#   [1] "INT1U"
# Slot "byteorder":
#   [1] "little"
# Slot "nodatavalue":
#   [1] -Inf
# Slot "NAchanged":
#   [1] FALSE
# Slot "nbands":
#   [1] 3
# Slot "bandorder":
#   [1] "BIL"
# Slot "offset":
#   [1] 0
# Slot "toptobottom":
#   [1] TRUE
# Slot "blockrows":
#   [1] 1
# Slot "blockcols":
#   [1] 3063
# Slot "driver":
#   [1] "gdal"
# Slot "open":
#   [1] FALSE

redline@file@name
# [1] "/cloud/project/products/figures/redlinemap.tif"
redline@file@datanotation
# [1] "INT1U"
redline@file@byteorder
# "little"
redline@file@nodatavalue
# [1] -Inf
redline@file@NAchanged
# FALSE
redline@file@nbands
# [1] 3
redline@file@bandorder
# [1] "BIL"
redline@file@offset
# [1] 0
redline@file@toptobottom
# [1] TRUE
redline@file@blockrows
# [1] 1
redline@file@driver
# [1] "gdal"
redline@file@open
# [1] FALSE


redline@data
# An object of class ".SingleLayerData"
# Slot "values":
#   logical(0)
# Slot "offset":
#   [1] 0
# Slot "gain":
#   [1] 1
# Slot "inmemory":
#   [1] FALSE
# Slot "fromdisk":
#   [1] TRUE
# Slot "isfactor":
#   [1] FALSE
# Slot "attributes":
#   list()
# Slot "haveminmax":
#   [1] TRUE
# Slot "min":
#   [1] 0
# Slot "max":
#   [1] 255
# Slot "band":
#   [1] 1
# Slot "unit":
#   [1] ""
# Slot "names":
#   [1] "redlinemap"

redline@data@values
# logical(0)
redline@data@offset
# [1] 0
redline@data@gain
# [1] 1
redline@data@inmemory
# [1] FALSE
redline@data@fromdisk
# [1] TRUE
redline@data@isfactor
# [1] FALSE
redline@data@attributes

redline@data@band
# [1] 1

line

redline@legend
# An object of class ".RasterLegend"
# Slot "type":
#   character(0)
# Slot "values":
#   logical(0)
# Slot "color":
#   logical(0)
# Slot "names":
#   logical(0)
# Slot "colortable":
#   logical(0)

redline@legend@type
# character(0)
redline@legend@values
# logical(0)
redline@legend@color
# logical(0)







line

redline@title
# character(0)

redline@extent
# class       : Extent 
# xmin        : 39.30517 
# xmax        : 39.31015 
# ymin        :-76.64618 
# ymax        :-76.64114 



redline@rotated
# [1] FALSE

redline@rotation
# An object of class ".Rotation"
# Slot "geotrans":
#   numeric(0)
# 
# Slot "transfun":
#   function () 
#     NULL
# <bytecode: 0x55bc7e9fc648>

redline@ncols
# 3063

redline@nrows
# 3103

redline@crs
# CRS arguments:
#   +proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs 

redline@history
# list()

redline@z
# list()