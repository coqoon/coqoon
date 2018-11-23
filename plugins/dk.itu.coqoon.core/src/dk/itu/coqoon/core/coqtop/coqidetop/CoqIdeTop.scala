package dk.itu.coqoon.core.coqtop.coqidetop

trait CoqIdeTop_v20170413 {
  def add(stateId : Integer, command : String, v : Interface.verbose) :
      Interface.value[
        (Interface.state_id, (Either[Unit, Interface.state_id], String))]
}
