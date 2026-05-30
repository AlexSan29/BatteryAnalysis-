ResistancePerCyclePlot <- function(PerCycleData){

  XMax <- max(PerCycleData$Cycle, na.rm = TRUE)
  XMajorBreaks <- seq(0,XMax, by = 1)
  
  YMaxRaw <- max(PerCycleData$Resistance, na.rm = TRUE)
  YMax <- ceiling(YMaxRaw / 50) * 50
  YMajorBreaks <- seq(0, YMax, by = 30)
  YMinorBreaks <- seq(0, YMax, by = 15)

  ResistancePlot <- PerCycleData  |>
    group_by(File, Cycle) |> 
    ggplot(aes(Cycle, Resistance, color = File)) +
    geom_point(size = 3) +
    geom_line(linewidth = 2) +
    
    scale_x_continuous(
      breaks       = XMajorBreaks, 
      limits       = c(1, XMax),
      labels       = label_number(accuracy = 1)) +
    
    scale_y_continuous(
      breaks       = YMajorBreaks,
      minor_breaks = YMinorBreaks,
      limits       = c(0, YMax),
      labels       = label_number(accuracy = 1),
      guide        = guide_axis(minor.ticks = TRUE)) +
    
    labs(
      x = "Cycle",
      y = "Resistance (Ω)",
      ) +
    Theme_AS()
  
  filename <- paste0("outputs/ResistanceCycle.png")
  ggsave(filename, ResistancePlot, width = 12, height = 6, dpi = 300)
  print(ResistancePlot)
  return(ResistancePlot)
}