import java.time.LocalDateTime
import scala.util.Random
import scala.io.StdIn._

// EnergyData case class
case class EnergyData(timestamp: LocalDateTime, energySource: String, output: Double, status: Boolean)

// Base trait for all energy sources
trait EnergySource {
  def generateData(): EnergyData
  def adjustOutput(factor: Double): Unit
  def toggleOnOff(status: Boolean): Unit
  def getName: String
}

// Solar Panel implementation
class SolarPanel(var baseOutput: Double, var isOn: Boolean = true) extends EnergySource {
  var currentOutput: Double = baseOutput

  override def generateData(): EnergyData = {
    if (isOn) {
      val fluctuation = Random.nextDouble() * 0.1
      currentOutput = baseOutput * (1 + fluctuation)
    } else {
      currentOutput = 0
    }
    EnergyData(LocalDateTime.now(), "Solar", currentOutput, isOn)
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

  override def getName: String = "Solar"
}

// Wind Turbine implementation
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

  override def getName: String = "Wind"
}

// Hydropower implementation
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

  override def getName: String = "Hydropower"
}