
! Note: maybe have multiple expectation types for subroutines, classes, etc.
!       Though this notion seems to have been moved to Predicates.
! 

module Expectation_mod
  use StringConversionUtilities_mod, only : MAXLEN_STRING
  use EventPolyWrapVector_mod
  use Predicates_mod
  implicit none
  private

  public :: Expectation, newExpectation, ExpectationThat
  public :: Predicate
  public :: Subject, newSubject

  type :: Subject
     ! mlr todo allocatable strings
     character(len=MAXLEN_STRING) :: name
     procedure(subVoid), pointer, nopass :: ptr
  end type Subject

  interface 
     subroutine subVoid
     end subroutine subVoid
  end interface

  interface newSubject
     module procedure newSubject_
     module procedure newSubjectNameOnly_
  end interface newSubject

! TDD
!  type(Predicate), parameter :: wasCalled     = Predicate('wasCalled')
!  type(Predicate), parameter :: wasNotCalled  = Predicate('wasNotCalled')
!  type(Predicate), parameter :: wasCalledOnce = Predicate('wasCalledOnce')
!
! todo:  
!    checking expectation sub called with right value (important for sci.)
!    syntax for distinguishing arguments -- (position/keys)
!    combined expectations -- one on method, one on argument
!    -- or combined in the text...
! todo expectation augment
!    - vary numbers & kinds of arguments 
! todo:  automatic generation -- for proposal
! todo:  a trivial example of interleaved method calls
! 
! todo question: !    how to require mock functions to return certain values

  type :: Expectation
     type(Subject) :: subj
     class(Predicate), pointer :: pred
   contains
     procedure :: verify_
     !procedure :: verify_i1_
     procedure :: verify_poly1_
     !generic   :: verify => verify_, verify_i1_
     generic   :: verify => verify_, verify_poly1_
     procedure :: argumentsToBeVerified_
     generic :: argumentsToBeVerified => argumentsToBeVerified_
  end type Expectation

  interface verify
     module procedure verify_
     module procedure verify_i1_
  end interface verify

  !interface argumentsToBeVerified
  !   module procedure argumentsToBeVerified_
  !end interface argumentsToBeVerified

contains
  type(Subject) function newSubject_(name,sub) result(subj_)
    character(*) :: name
    procedure(subVoid), pointer :: sub
    subj_%name = name
    subj_%ptr => sub
    ! maybe include a reference too
  end function newSubject_

  type(Subject) function newSubjectNameOnly_(name) result(subj_)
    character(*) :: name
    procedure(subVoid), pointer :: sub
    subj_%name = name
    ! subj_%ptr => sub ! Maybe nullify...
    nullify(subj_%ptr)
    ! maybe include a reference too
  end function newSubjectNameOnly_

!  type(Subject) function newSubject(name) result(subj_)

  type(Expectation) function newExpectation(subj, pred) result(expect)
    type(Subject), intent(in) :: subj
    class(Predicate), intent(in), target :: pred
    !print *,3000,trim(subj%name)//'...'//trim(pred%name)
    expect%subj = subj
    ! Pointer or copy?
    expect%pred => pred
  end function newExpectation

  type(Expectation) function ExpectationThat(subject,pred) result(expect)
    character(*) :: subject
    class(Predicate) :: pred
    expect = newExpectation(newSubject(subject),pred)
  end function ExpectationThat

  ! How to make abstract?
  logical function verify_(this,eventList) result(ok)
    use Exception_mod
    class (Expectation), intent(inout) :: this
    type (EventPolyWrapVector), intent(in) :: eventList
    !print *,4000,trim(this%subj%name)//'...'//trim(this%pred%name)
    ok = this%pred%verify(this%subj%name,eventList)
  end function verify_

  logical function verify_i1_(this,eventList,i1) result(ok)
    use Exception_mod
    class (Expectation), intent(inout) :: this
    type (EventPolyWrapVector), intent(in) :: eventList
    integer, intent(in) :: i1
    !print *,5000,trim(this%subj%name)//'...'//trim(this%pred%name)
    ok = this%pred%verify(this%subj%name,eventList,i1)
  end function verify_i1_

  logical function verify_poly1_(this,eventList,p1) result(ok)
    use Exception_mod
    class (Expectation), intent(inout) :: this
    type (EventPolyWrapVector), intent(in) :: eventList
    class (*), intent(in) :: p1
    ok = this%pred%verify(this%subj%name,eventList,p1)
  end function verify_poly1_

  logical function argumentsToBeVerified_(this)
    class (Expectation), intent(in) :: this
    !type (Expectation), intent(in) :: this
    argumentsToBeVerified_ = this%pred%argumentsToBeVerified()
  end function argumentsToBeVerified_
  
end module Expectation_mod
