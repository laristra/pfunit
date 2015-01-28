module Predicates_mod
  
  use Predicate_mod, only: Predicate
  use Predicate_mod, only: finalVerificationEnabled
  use Predicate_mod, only: finalVerificationEnabledMessage
  
  use PredicateWasCalled_mod, only:       WasCalled
  use PredicateWasCalledOnce_mod, only:   WasCalledOnce
  use PredicateWasNotCalled_mod, only:    WasNotCalled
  use PredicateArgumentsEqual_mod, only:  ArgumentsEqual
  
  implicit none
  private

  public Predicate
  public finalVerificationEnabled
  public finalVerificationEnabledMessage

  ! Need to separate the finals vs. the argument verifiers...
  !
  public WasCalled
  public WasCalledOnce
  public WasNotCalled
  public ArgumentsEqual

  ! Maybe I can put argument info into extended events rather than
  ! passing things through a call chain...

end module Predicates_mod
