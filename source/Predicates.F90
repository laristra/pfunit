module Predicates_mod
  
  use Predicate_mod, only: Predicate
  !, newPredicate
  
  use PredicateWasCalled_mod, only:     WasCalled
  use PredicateWasCalledOnce_mod, only: WasCalledOnce
  
  ! uze PredicateWasCalled_mod, only: WasCalled, nWasCalled
  ! PredicateWasNotCalled_mod, only: WasNotCalled
  implicit none
  private

  public Predicate
  ! , newPredicate

  public WasCalled
  public WasCalledOnce

!  public WasCalled, nWasCalled
!  public WasNotCalled

end module Predicates_mod
