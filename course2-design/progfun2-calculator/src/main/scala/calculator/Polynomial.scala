package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
    if (delta() < 0) Set()
    else if (a() == 0 && c() == 0) Set(0)
      else if (a() == 0 && c() != 0) Set(-1 * (c() / b()))
        else Set(
            (-1 * b() + Math.sqrt(delta())) / 2 * a(),
            (-1 * b() - Math.sqrt(delta())) / 2 * a()
        )
    }
  }
}
