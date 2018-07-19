import groovy.swing.SwingBuilder
import javax.swing.JFrame
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.plot.PlotOrientation

def chart = {
    x = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    y = [2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0]

    def series = new XYSeries('plots')
    [x, y].transpose().each { x, y -> series.add x, y }

    def labels = ["Plot Demo", "X", "Y"]
    def data = new XYSeriesCollection(series)
    def options = [false, true, false]

    def chart = ChartFactory.createXYLineChart(*labels, data, PlotOrientation.VERTICAL, *options)
    new ChartPanel(chart)
}

new SwingBuilder().edt {
    frame(title:'Plot coordinate pairs', defaultCloseOperation:JFrame.EXIT_ON_CLOSE, pack:true, show:true) {
        widget(chart())
    }
}
