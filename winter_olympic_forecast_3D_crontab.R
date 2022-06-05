#==========================================================
#
# 本程序用于批量制作冬奥1km/3km分辨率模式预报的三维图形产品.
#
# * 从Cassandra服务器上读取CMA_MESO_1km数据气温和10米U/V风速.
# * 采用rayshader将温度场和风流线场叠加到三维场上.
# * 在命令窗口中运行:
# *     R --vanilla --verbose CMD BATCH /media/kan-dai/workspace/workcode/personal/dk_winter_olympic_3D/winter_olympic_forecast_3D_crontab.R
# * 如果要在crontab中进行定时作业, 请使用 winter_olympic_forecast_3D_crontab.sh 文件
# > crontab -e
#   50 * * * * (flock winter_olympic_forecast_3D_crontab.lock "/media/kan-dai/workspace/workcode/personal/dk_winter_olympic_3D/winter_olympic_forecast_3D_crontab.sh")
#   注意一定要加 "export DISPLAY=:1" (这里1为显示器序号, 每台机器不同, 网上说的0就不行)
#   如果使用render_snapshot(webshot=TRUE), 会运行很慢, 且字体不能正常显示.
#==========================================================

#==========================================================
# 载入所有用到的软件库
library(ggplot2)
library(stringr)
library(raster)
library(png)
library(rayshader)
library(leaflet)
library(metR)
library(geosphere)
library(magrittr)

# 从Cassandra服务器上读取模式数据
# 安装命令, devtools::install_github("nmcdev/nmcMetIO")
library(nmcMetIO)


#==========================================================
# 设置本程序用到的文件及相关参数

#
# 以下参数需要手工配置

# 设置工作目录(请使用绝对路径, 其后的产品都在该目录下运行)
work_dir <- "/media/kan-dai/workspace/workcode/personal/dk_winter_olympic_3D"
setwd(work_dir)

# 设置图片产品的输出目录
# sudo mount -t cifs -o username=share,password=nmcwft,dir_mode=0755,file_mode=0755,uid=1000,gid=1000 //10.28.16.160/public /media/winter_olympic_160
# output_dir <- file.path(work_dir, 'data', 'winter_olympic_images')
output_dir <- "/media/winter_olympic_160"
#output_dir <- "/media/kan-dai/workspace/workcode/personal/dk_winter_olympic_3D/products"

# 设置GMOSRR数据目录
# \\10.20.90.104\winter_olympic\GRD\NMCGRID-GMOSRR
GMOSRR <- "/media/winter_olympic_gmosrr"

# 地形数据文件,数据提前从下面网站下载, 输出为geotiff格式, 分辨率选择最大
# https://www.gmrt.org/services/gridserverinfo.php#!/services/getGMRTGrid
# 在工作目录下建立 /data/winter_olympic的目录, 把地形文件拷贝到该目录即可
elev_file <- file.path(work_dir, 'data',"stadium_topo.tif")

#
# 以下参数不用手工配置

# 地图图像文件,如果该文件不存在,则会采用nmcMetIO::get_arcgis_map_image从
# Arcgis网站上自动下载.
map_file <- file.path(work_dir, 'data', "stadium_map.png")

# 地形阴影数据文件, 如果文件不存在, 程序会自动生成.
shademat_file <- file.path(work_dir, 'data', "shademat.rds")
ambmat_file <- file.path(work_dir, 'data', "ambmat.rds")
raymat_file <- file.path(work_dir, 'data', "raymat.rds")


#==========================================================
# 读入和处理地形数据

# load topography data
elev_img <- raster(elev_file)

# set coordinates
crs(elev_img) <- CRS('+init=EPSG:4326')

# convert it to a matrix which rayshader can handle.
elev_matrix <- raster_to_matrix(elev_img)

# fill NA values
elev_matrix[!is.finite(elev_matrix)] <- min(elev_matrix, na.rm=TRUE)


#============================
# 准备地图图像文件

# get map extent
bbox = extent(elev_img)
bbox = list(p1 = list(long = bbox[1], lat = bbox[3]),
            p2 = list(long = bbox[2], lat = bbox[4]))

# get map image size
image_size <- define_image_size(bbox, major_dim = 2000)

# 下载和读入地图图形文件
if(!file.exists(map_file)){
  get_arcgis_map_image(bbox, map_type = "World_Topo_Map", file = map_file,
                       width = image_size$width, height = image_size$height, 
                       sr_bbox=4326)}

#============================
# 准备地形阴影数据

if(!file.exists(shademat_file)){
  shademat <- sphere_shade(elev_matrix, texture = "imhof4")
  saveRDS(shademat, file=shademat_file)}
shademat <- readRDS(shademat_file)

if(!file.exists(ambmat_file)){
  ambmat <- ambient_shade(elev_matrix, zscale=30)
  saveRDS(ambmat, file=ambmat_file)}
ambmat <- readRDS(ambmat_file)

if(!file.exists(raymat_file)){
  raymat <- ray_shade(elev_matrix, zscale=30, lambert = TRUE)
  saveRDS(raymat, file=raymat_file)}
raymat <- readRDS(raymat_file)


#==========================================================
# 循环每个预报场, 生成相应的图片产品


# 设置模式名称
model_names = c('CMA_MESO(1km)', 'CMA_MESO(3km)')

for (model in model_names){
  
  # 获得模式的最新起报时间
  if (model == 'CMA_MESO(1km)'){
    filename <- gds_get_latest_filename("GRAPES_1KM/TMP/2M_ABOVE_GROUND/",
                                        filter="*.001")
    fhours <- seq(1:24)
  } else {
    filename <- gds_get_latest_filename("GRAPES_3KM/TMP/2M_ABOVE_GROUND/",
                                        filter="*.001")
    fhours <- seq(0:72)
  }
  basefile <- sub('\\..[^\\.]*$', '', filename)
  
  # 构建输出文件夹
  output_subdir <- file.path(output_dir, model, basefile)
  if (!dir.exists(output_subdir)){
    dir.create(output_subdir, recursive=TRUE)}
  
  # 循环每个预报时效
  for (fhour in fhours){
    # 构建数据文件名
    filename <- paste0(basefile, '.', str_pad(fhour,3,pad="0"))
    
    # 构建输出图形文件名, 并检查是否已经生成
    outfilename <- file.path(output_subdir, paste0(filename,'.png'))
    if (file.exists(outfilename)){
      next}
    
    #============================
    # 制作温度场填充等值线图
    if (model == 'CMA_MESO(1km)'){
      dataT <- retrieve_micaps_model_grid("GRAPES_1KM/TMP/2M_ABOVE_GROUND/", filename=filename)
    } else {
      dataT <- retrieve_micaps_model_grid("GRAPES_3KM/TMP/2M_ABOVE_GROUND/", filename=filename)
    }
    if (is.null(dataT)) next
    
    # extract time information
    init_time_str <- format(dataT$initTime[1], '%Y/%m/%dT%H')
    valid_time_str <- format(dataT$time[1], '%Y/%m/%dT%H')
    fhour_str <- str_pad(fhour,3,pad="0")
    
    # extract region
    dataT <- dataT[lon >= bbox$p1$long & lon <= bbox$p2$long & lat >= bbox$p1$lat & lat <= bbox$p2$lat]
    
    # smooth the data
    out <- smooth2d(dataT$lon, dataT$lat, dataT$var1, theta=0.02, nx=128, ny=128)
    
    # plot contour image
    colors <- c("#3D0239","#FA00FC","#090079","#5E9DF8","#2E5E7F",
                "#06F9FB","#0BF40B","#006103","#FAFB07","#D50404","#5A0303")
    p_temp <- ggplot(out, aes(x, y, z=z)) +
      geom_contour_fill(breaks=seq(-16, 12, by=0.25)) +
      geom_contour(breaks=seq(-16, 8, by=0.25), color="black", size=0.5) + 
      scale_fill_gradientn(name="Temp", colours=colors, limits=c(-12, 8)) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      guides()+
      theme(panel.spacing=grid::unit(0, "mm"),
            plot.margin=grid::unit(rep(-1.25,4),"lines"),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            legend.key.size = unit(5, "cm"),
            legend.key.height = unit(2.5, 'cm'),
            legend.key.width = unit(6, 'cm'),
            legend.position = c(0.25, 0.06),
            legend.direction="horizontal",
            legend.text = element_text(size=32),
            legend.title = element_text(size=32),
            legend.background = element_rect(fill="lightblue", 
                                             size=0.5, linetype="solid"))
    
    # export png file
    overlay_file_temp <- file.path(output_dir,paste0(model,"_temperature_image.png"))
    png(overlay_file_temp, width=image_size$width, height=image_size$height,units="px", bg="transparent")
    print(p_temp)
    dev.off()
    
    #============================
    # 制作风场流线图
    # load fine gridded forecast
    if (model == 'CMA_MESO(1km)'){
      dataU <- retrieve_micaps_model_grid("GRAPES_1KM/UGRD/10M_ABOVE_GROUND/", filename=filename)
      if (is.null(dataU)) next
      dataV <- retrieve_micaps_model_grid("GRAPES_1KM/VGRD/10M_ABOVE_GROUND/", filename=filename)
      if (is.null(dataV)) next
    } else {
      dataU <- retrieve_micaps_model_grid("GRAPES_3KM/UGRD/10M_ABOVE_GROUND/", filename=filename)
      if (is.null(dataU)) next
      dataV <- retrieve_micaps_model_grid("GRAPES_3KM/VGRD/10M_ABOVE_GROUND/", filename=filename)
      if (is.null(dataV)) next
    }
    
    # compute wind speed at points
    point = list(lon=116.39200, lat=39.99150)
    pointU = dataU[, Interpolate(var1 ~ lon + lat, point$lon, point$lat, grid=FALSE)]
    pointV = dataV[, Interpolate(var1 ~ lon + lat, point$lon, point$lat, grid=FALSE)]
    pointWS = sqrt(pointU$var1 * pointU$var1 + pointV$var1 * pointV$var1)
    
    # extract region
    dataU <- dataU[lon >= bbox$p1$long & lon <= bbox$p2$long & lat >= bbox$p1$lat & lat <= bbox$p2$lat]
    dataV <- dataV[lon >= bbox$p1$long & lon <= bbox$p2$long & lat >= bbox$p1$lat & lat <= bbox$p2$lat]
    dataUV <- merge(dataU, dataV, by=c("lon", "lat", "lev", "time", "initTime", "fhour"))
    
    # plot wind streamlines
    p_wind <- ggplot(dataUV, aes(lon, lat)) +
      geom_arrow(aes(dx = dataUV$var1.x*0.2, dy = dataUV$var1.y*0.2), 
                 skip = 1, color = "white", arrow.length=1.5, size = 2) +
      geom_streamline(aes(dx=dlon(dataUV$var1.x,lat), dy=dlat(dataUV$var1.y), size=..step.., 
                          alpha=..step.., color=sqrt(..dx..^2 + ..dy..^2)),
                      L=0.05, res=3, arrow=NULL, n=25, S=NULL, lineend="round", size=5) + 
      viridis::scale_color_viridis(guide="none") +
      scale_size(range=c(0,3), guide="none") +
      scale_alpha(guide="none") +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme(panel.spacing=grid::unit(0, "mm"),
            plot.margin=grid::unit(rep(-1.25,4),"lines"),
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank())
    
    # export png file
    overlay_file_wind <- file.path(output_dir,paste0(model,"_wind_streamline_image.png"))
    png(overlay_file_wind, width=image_size$width, height=image_size$height, units="px", bg="transparent")
    print(p_wind)
    dev.off()
    
    #============================
    # 制作三维地图
    
    # 读入图形数据
    overlay_img <- readPNG(map_file)
    overlay_img_temp <- png::readPNG(overlay_file_temp)
    overlay_img_wind <- png::readPNG(overlay_file_wind)
    
    # 设置垂直尺度
    zscale <- 10
    
    # 设置标题
    title <- paste0("2022北京冬奥",model," 风场及温度预报(",valid_time_str,")\n",
                    "起报: ",init_time_str, "; 时效: ",fhour_str)
    
    rgl::clear3d()
    shademat %>% 
      add_overlay(overlay_img, alphalayer = 0.95) %>%
      add_overlay(overlay_img_temp, alphalayer = 0.6) %>%
      add_overlay(overlay_img_wind, alphalayer = 0.8) %>%
      add_shadow(raymat, max_darken = 0.4) %>%
      add_shadow(ambmat, max_darken = 0.4) %>%
      plot_3d(elev_matrix, zscale = zscale, windowsize = c(1000, 1000), background = "white", 
              water = FALSE, soliddepth = -150, wateralpha = 0)
    
    label <- list(text=paste0("Olympic Stadium: ", as.character(round(pointWS,digits=2)), "m/s"))
    label$pos <- find_image_coordinates(long=point$lon, lat=point$lat, bbox=bbox, 
                                        image_width=dim(elev_matrix)[1], image_height=dim(elev_matrix)[2])
    render_label(elev_matrix, x = label$pos$x, y = label$pos$y, z = 6000,
                 zscale = zscale, text = label$text, textsize=1.2, linewidth=5, freetype=TRUE)
    dist = distm(c(extent(elev_img)[1], extent(elev_img)[3]), 
                 c(extent(elev_img)[1], extent(elev_img)[4]), fun = distHaversine)[,1]
    # render_scalebar(limits=round(dist/1000.,1),label_unit = 'km')
    render_compass(position = "N",compass_radius=120, scale_distance = 1.2)
    
    rgl::par3d(windowRect=c(-450, -100, 1000, 1100))
    rgl::view3d(theta=20, phi=30, fov=45, zoom=0.6)
    
    render_snapshot(outfilename, title_text = title, instant_capture=FALSE, bring_to_front=TRUE,
                    title_size = 45, title_color = "black", title_font = "SimHei")
  }
}

# close rgl window
rgl::rgl.close()
