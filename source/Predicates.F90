module Predicates_mod
  
  use Predicate_mod, only: Predicate, newPredicate
  use PredicateWasCalled_mod, only: WasCalled, nWasCalled
  ! PredicateWasNotCalled_mod, only: WasNotCalled
  ! PredicateWasCalledOnce_mod, only: WasCalledOnce
  implicit none
  private

  public Predicate, newPredicate
  public WasCalled, nWasCalled
!  public WasNotCalled
!  public WasCalledOnce

end module Predicates_mod
