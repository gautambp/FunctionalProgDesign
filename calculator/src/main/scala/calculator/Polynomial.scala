package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal[Double]((b.apply() * b.apply()) - (4 * a.apply() * c.apply()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    if (delta.apply() < 0) new Signal[Set[Double]](Set[Double]())
    else {
      val p1 = ((-1.0 * b.apply() + math.sqrt(delta.apply())) / (2.0 * a.apply()))
      val p2 = ((-1.0 * b.apply() - math.sqrt(delta.apply())) / (2.0 * a.apply()))
      new Signal[Set[Double]] (Set(p1, p2))
    }
  }
}
