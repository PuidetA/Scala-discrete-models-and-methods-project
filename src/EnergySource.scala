import java.time.LocalDateTime
import scala.util.Random
import scala.io.StdIn._

trait EnergySource {
  def generateData(): EnergyData
  def adjustOutput(factor: Double): Unit
  def toggleOnOff(status: Boolean): Unit
}

case class EnergyData(timestamp: LocalDateTime, energySource: String, output: Double, status: Boolean)

class SolarPanel(var baseOutput: Double, var isOn: Boolean = true) extends EnergySource {
  var currentOutput: Double = baseOutput  // Current output starts at base output

  override def generateData(): EnergyData = {
    if (isOn) {
      val fluctuation = Random.nextDouble() * 0.1
      currentOutput = baseOutput * (1 + fluctuation)  // Fluctuate based on base output
    } else {
      currentOutput = 0
    }
    EnergyData(LocalDateTime.now(), "Solar", currentOutput, isOn)
  }

  override def adjustOutput(factor: Double): Unit = {
    if (isOn) {
      baseOutput *= factor
      currentOutput = baseOutput  // Adjust current output immediately to reflect change
    }
  }

  override def toggleOnOff(status: Boolean): Unit = {
    isOn = status
    if (isOn) {
      baseOutput *= (0.8 + Random.nextDouble() * 0.4)  // Reset base output to a reasonable value
      currentOutput = baseOutput
    } else {
      currentOutput = 0
    }
  }
}

class WindTurbine(var baseOutput: Double, var isOn: Boolean = true) extends EnergySource {
  var currentOutput: Double = baseOutput

  override def generateData(): EnergyData = {
    if (isOn) {
      val fluctuation = Random.nextDouble() * 0.2
      currentOutput = baseOutput * (1 + fluctuation)
    } else {
      currentOutput = 0
    }
    EnergyData(LocalDateTime.now(), "Wind", currentOutput, isOn)
  }

  override def adjustOutput(factor: Double): Unit = {
    if (isOn) {
      baseOutput *= factor
      currentOutput = baseOutput
    }
  }

  override def toggleOnOff(status: Boolean): Unit = {
    isOn = status
    if (isOn) {
      baseOutput *= (0.8 + Random.nextDouble() * 0.4)
      currentOutput = baseOutput
    } else {
      currentOutput = 0
    }
  }
}

class Hydropower(var baseOutput: Double, var isOn: Boolean = true) extends EnergySource {
  var currentOutput: Double = baseOutput

  override def generateData(): EnergyData = {
    if (isOn) {
      val fluctuation = Random.nextDouble() * 0.15
      currentOutput = baseOutput * (1 + fluctuation)
    } else {
      currentOutput = 0
    }
    EnergyData(LocalDateTime.now(), "Hydropower", currentOutput, isOn)
  }

  override def adjustOutput(factor: Double): Unit = {
    if (isOn) {
      baseOutput *= factor
      currentOutput = baseOutput
    }
  }

  override def toggleOnOff(status: Boolean): Unit = {
    isOn = status
    if (isOn) {
      baseOutput *= (0.8 + Random.nextDouble() * 0.4)
      currentOutput = baseOutput
    } else {
      currentOutput = 0
    }
  }
}


