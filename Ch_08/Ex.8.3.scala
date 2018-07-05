trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop =
    new Prop {
      def check: Boolean = this.check && p.check
    }
}