module Predicates_mod
  
  use Predicate_mod, only: Predicate
  
  use PredicateWasCalled_mod, only:     WasCalled
  use PredicateWasCalledOnce_mod, only: WasCalledOnce
  use PredicateWasNotCalled_mod, only:     WasNotCalled
  
  implicit none
  private

  public Predicate

  public WasCalled
  public WasCalledOnce
  public WasNotCalled


end module Predicates_mod
