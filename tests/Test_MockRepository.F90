!#include "reflection.h"
#define HERE print*,__LINE__,__FILE__

! Note: The SUT is not really the thing being mocked. Some
! reference or dependency in a SUT is what's being mocked.
! We'll likely change this at some point. 2014-0919-1541-45-UTC MLR
!
module SUT_InternalDependency_mod
  use AssertArraysSupport_mod, only: differenceReport, valuesReport
  use ThrowFundamentalTypes_mod, only: locationFormat
  use SourceLocation_mod
  
   implicit none
   private

   public :: SUT_InternalDependency
   public :: newSUT_InternalDependency
   public :: newSUT_InternalDependencyPointer   

   type SUT_InternalDependency
      integer :: intComponent
   contains
      procedure :: method1
      procedure :: method2
      procedure :: method3
      procedure :: method4
      procedure :: method5
   end type SUT_InternalDependency

 contains

   function newSUT_InternalDependency() result(SUT)
     type (SUT_InternalDependency), allocatable :: SUT
     allocate(SUT)
   end function newSUT_InternalDependency

   function newSUT_InternalDependencyPointer() result(SUT)
     type (SUT_InternalDependency), pointer :: SUT
     allocate(SUT)
   end function newSUT_InternalDependencyPointer

   subroutine method1(this)
     class (SUT_InternalDependency), intent(in) :: this
   end subroutine method1

   subroutine method2(this)
     class (SUT_InternalDependency), intent(in) :: this
   end subroutine method2

   subroutine method3(this)
     class (SUT_InternalDependency), intent(in) :: this
   end subroutine method3

   subroutine method4(this,i)
     class (SUT_InternalDependency), intent(in) :: this
     integer, intent(in) :: i
   end subroutine method4

   subroutine method5(this,r)
     class (SUT_InternalDependency), intent(in) :: this
     real, intent(in) :: r
   end subroutine method5

end module SUT_InternalDependency_mod

module MockSUT_InternalDependency_mod
   use MockRepository_mod
   use SUT_InternalDependency_mod
   use Exception_mod
   implicit none
   private

   public :: MockSUT_InternalDependency
   public :: newMockSUT_InternalDependency
   public :: newMockSUT_InternalDependencyPointer

   type, extends(SUT_InternalDependency) :: MockSUT_InternalDependency
   contains
      procedure :: method1 ! mocked
      procedure :: method2 ! not mocked
      procedure :: method3 ! mocked
      procedure :: method4 ! mocked
      procedure :: method5 ! mocked      
   end type MockSUT_InternalDependency

contains

   function newMockSUT_InternalDependency() result(SUT_withMock)
     type (MockSUT_InternalDependency), allocatable :: SUT_withMock
     allocate(SUT_withMock)
   end function newMockSUT_InternalDependency

   function newMockSUT_InternalDependencyPointer() result(SUT_withMock)
     type (MockSUT_InternalDependency), pointer:: SUT_withMock
     allocate(SUT_withMock)
   end function newMockSUT_InternalDependencyPointer
   

   subroutine method1(this)
      class (MockSUT_InternalDependency), intent(in) :: this
      call MockRepositoryPointer&
!           &%verifyArguments('MockSUT_InternalDependency%method1')
           &%registerMockCallBy('MockSUT_InternalDependency%method1')
! TODO: Do we want to register the call, and then search for an expectation on the arguments to verify?
!!!      call MockRepositoryPointer&
!!!           &%verifyArguments('MockSUT_InternalDependency%method1')

! TODO: fix

   end subroutine method1

   subroutine method2(this)
     class (MockSUT_InternalDependency), intent(in) :: this
     call this%SUT_InternalDependency%method2()  ! SUT_InternalDependency not abstract!
   end subroutine method2

   subroutine method3(this)
     class (MockSUT_InternalDependency), intent(in) :: this
      call MockRepositoryPointer&
           &%registerMockCallBy('MockSUT_InternalDependency%method3')
   end subroutine method3

! TODO: New verification scheme.
!
   subroutine method4(this,i)
     class (MockSUT_InternalDependency), intent(in) :: this
     integer, intent(in) :: i
     call MockRepositoryPointer&
          &%registerMockCallBy('MockSUT_InternalDependency%method4')
     call MockRepositoryPointer&
          &%verifyArguments('MockSUT_InternalDependency%method4',i)

     ! Note:  Since the above in unlimited polymorphic, if you have
     !        multiple arguments, then you can create an object with those
     !        arguments as slots, and then pass that object into
     !        verifyArguments, "unpacking" them in the predicate.
     !
     !        This may help passing and working with arrays.
     
     ! TODO:  
     !Q?      Throw an exception -- how to unwind?  Kick back?  
     !Q?      The SUT won't know...  The mocks & the system will...  Wait for the SUT to return?

     ! The following might be the way to return from the SUT.
     !??? if (anyExceptions()) return

! Call other stuff...
   end subroutine method4

   subroutine method5(this,r)
     class (MockSUT_InternalDependency), intent(in) :: this
     real, intent(in) :: r
     call MockRepositoryPointer&
          &%registerMockCallBy('MockSUT_InternalDependency%method5')
     call MockRepositoryPointer&
          &%verifyArguments('MockSUT_InternalDependency%method5',r)
   end subroutine method5


end module MockSUT_InternalDependency_mod

module Test_MockRepository_mod
   use TestSuite_mod
   use MockRepository_mod
   use Assert_mod
   use SourceLocation_mod
   use Exception_mod, only : NULL_MESSAGE, catch, anyExceptions
   use Expectation_mod
   use Predicates_mod

!   use Assert_mod, only: assertEqual

   use SUT_InternalDependency_mod
   use MockSUT_InternalDependency_mod
   implicit none
   private

   public :: suite

   ! test that all registered objects are finalized (checked)

contains

!#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

   function suite()
      use TestSuite_mod, only: newTestSuite, TestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('Test_MockRepository')

      call suite%addTest( &
           &   newTestMethod('testNoAction', &
           &                  testNoAction))
      call suite%addTest( &
           &   newTestMethod('testExpectMethod_NotCalled', &
           &                  testExpectMethod_NotCalled))
      call suite%addTest( &
           &   newTestMethod('testExpectMethod_IsCalled', &
           &                  testExpectMethod_IsCalled))
      call suite%addTest( &
           &   newTestMethod('testExpectMethod_CalledDifferentMethod', &
           &                  testExpectMethod_CalledDifferentMethod))

      call suite%addTest( &
           &   newTestMethod('testExpectMethod_CalledOnce', &
           &                  testExpectMethod_CalledOnce))

      call suite%addTest( &
           &   newTestMethod('testExpectMethod_ExpectedOnceCalledTwice', &
           &                  testExpectMethod_ExpectedOnceCalledTwice))

      call suite%addTest( &
           &   newTestMethod('testExpectMethod_ExpectedZero', &
           &                  testExpectMethod_ExpectedZero))

      call suite%addTest( &
           &   newTestMethod('testExpectMethod_ExpectedZeroCalledOnce', &
           &                  testExpectMethod_ExpectedZeroCalledOnce))

      call suite%addTest ( &
           &   newTestMethod('testArgumentConstraint1', &
           &                  testArgumentConstraint1))

      call suite%addTest ( &
           &   newTestMethod('testArgumentConstraint2', &
           &                  testArgumentConstraint2))

      call suite%addTest ( &
           &   newTestMethod('testArgumentConstraint3', &
           &                  testArgumentConstraint3))

      
   end function suite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

   subroutine testNoAction()
     !type (SUT_InternalDependency) :: object
     !type (MockSUT_InternalDependency) :: SUT_withMockedMethod
     ! gcc-4.5-2, breaks intel-15
     !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
     !
     class (SUT_InternalDependency), pointer :: SUT_withMockedMethod
      
      !print *,1000
      !SUT_withMockedMethod = newMockSUT_InternalDependency()
      ! gcc-4.5-2 works, breaks intel-15
      !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
      SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
      !print *,1100
      call MockRepositoryPointer%verify()

   end subroutine testNoAction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

   subroutine testExpectMethod_NotCalled()

     !print *,2000
     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method1', &
          &   WasCalled()))
     call internalProcedure() ! verification is when object is final-ized

     call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
     call MockRepositoryPointer%verify()

     call assertCatch('"MockSUT_InternalDependency%method1" "wasCalled" does not hold.')

   contains

      subroutine internalProcedure()
        !type (MockSUT_InternalDependency) :: SUT_withMockedMethod
        !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
        class (SUT_InternalDependency), pointer :: SUT_withMockedMethod
         !!!print *,2100
        !SUT_withMockedMethod = newMockSUT_InternalDependency()
        !print *,2100
        !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
        SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
        !print *,2200
      end subroutine internalProcedure

   end subroutine testExpectMethod_NotCalled

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

   subroutine testExpectMethod_IsCalled()

     use Exception_mod, only : anyExceptions

     !print *,3000
     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method1', &
          &   WasCalled()))
     call internalProcedure() ! verification is when object is final-ized

     call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
     call MockRepositoryPointer%verify()

   contains

      subroutine internalProcedure()
        !type (MockSUT_InternalDependency) :: SUT_withMockedMethod
        !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
        class (SUT_InternalDependency), pointer :: SUT_withMockedMethod
        !!print *,3100
        !SUT_withMockedMethod = newMockSUT_InternalDependency()
        !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
        SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
        call SUT_withMockedMethod%method1()
      end subroutine internalProcedure

   end subroutine testExpectMethod_IsCalled


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
   subroutine testExpectMethod_CalledDifferentMethod()

     !print *,4000
     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method1', &
          &   WasCalled()))
     call internalProcedure() ! verification is when object is final-ized
      
     call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
     call MockRepositoryPointer%verify()

      call assertCatch('"MockSUT_InternalDependency%method1" "wasCalled" does not hold.')

    contains

      subroutine internalProcedure()
        !type (MockSUT_InternalDependency) :: SUT_withMockedMethod
        !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
        class (SUT_InternalDependency), pointer :: SUT_withMockedMethod
        !!print *,4100
        !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
        SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
        call SUT_withMockedMethod%method3()
      end subroutine internalProcedure

    end subroutine testExpectMethod_CalledDifferentMethod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
    subroutine testExpectMethod_CalledOnce()

      !print *,4000
      call MockRepositoryPointer%add( &
           & ExpectationThat( &
           &   'MockSUT_InternalDependency%method1', &
           &   WasCalledOnce()))
      call internalProcedure() ! verification is when object is final-ized
      
      call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
      call MockRepositoryPointer%verify()
      
!      call assertCatch('"MockSUT_InternalDependency%method1" "wasCalled" does not hold.')
      
    contains
      
      subroutine internalProcedure()
        !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
        class (SUT_InternalDependency), pointer :: SUT_withMockedMethod
        !!print *,4100
        !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
        SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
        call SUT_withMockedMethod%method1()
      end subroutine internalProcedure

    end subroutine testExpectMethod_CalledOnce

    subroutine testExpectMethod_ExpectedOnceCalledTwice()

      !print *,5000
      call MockRepositoryPointer%add( &
           & ExpectationThat( &
           &   'MockSUT_InternalDependency%method1', &
           &   WasCalledOnce()))
      call internalProcedure() ! verification is when object is final-ized
      
      call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
      call MockRepositoryPointer%verify()
      
      call assertCatch('"MockSUT_InternalDependency%method1" "wasCalledOnce" does not hold.')
      
    contains
      
      subroutine internalProcedure()
        !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
        class (SUT_InternalDependency), pointer :: SUT_withMockedMethod
        
        !!print *,4100
        !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
        SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
        call SUT_withMockedMethod%method1()
        call SUT_withMockedMethod%method1()
        
      end subroutine internalProcedure

    end subroutine testExpectMethod_ExpectedOnceCalledTwice

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

    subroutine testExpectMethod_ExpectedZero()

      !print *,6000
      call MockRepositoryPointer%add( &
           & ExpectationThat( &
           &   'MockSUT_InternalDependency%method1', &
           &   WasNotCalled()))
      call internalProcedure() ! verification is when object is final-ized
      
      call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
      call MockRepositoryPointer%verify()
      
!      call assertCatch('"MockSUT_InternalDependency%method1" "wasCalled" does not hold.')
      
    contains
      
      subroutine internalProcedure()
        !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
        class (SUT_InternalDependency), pointer :: SUT_withMockedMethod
        
        !!print *,4100
        !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
        SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
        call SUT_withMockedMethod%method3()
        
      end subroutine internalProcedure

    end subroutine testExpectMethod_ExpectedZero

    
    subroutine testExpectMethod_ExpectedZeroCalledOnce()

      !print *,7000
      call MockRepositoryPointer%add( &
           & ExpectationThat( &
           &   'MockSUT_InternalDependency%method1', &
           &   WasNotCalled()))
      call internalProcedure() ! verification is when object is final-ized
      
      call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
      call MockRepositoryPointer%verify()
      
!      call assertCatch('"MockSUT_InternalDependency%method1" "wasCalled" does not hold.')
      
    contains
      
      subroutine internalProcedure()
        !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
        class (SUT_InternalDependency), pointer :: SUT_withMockedMethod
        
        !!print *,4100
        !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
        SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
        call SUT_withMockedMethod%method3()
        
      end subroutine internalProcedure

    end subroutine testExpectMethod_ExpectedZeroCalledOnce

    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

! TODO:  Need a verify/catch to call between executions of mocks...
!        
!        call sut(mock1)
!        call catch & quit
!        call sut(mock2)
!        call catch & quit
!        call sut(mock3)
!        call catch & quit
!
! QUESTIONS:  What about calls like, call sut(mock1, mock2, mock3...)

   subroutine testArgumentConstraint1()

     !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
     class (SUT_InternalDependency), pointer :: SUT_withMockedMethod

     !print *,8000

     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          & 'MockSUT_InternalDependency%method4', &
          & argumentsEqual(999)))

     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method4', &
          &   WasCalled()))
     
     !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
     SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
     
     call SUT_withMockedMethod%method4(999)
     
     call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
     
     call MockRepositoryPointer%verify() ! Wrap up at end of tests...

   end subroutine testArgumentConstraint1


   subroutine testArgumentConstraint2()

!     type (MockSUT_InternalDependency) :: SUT_withMockedMethod
!     class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
     class (SUT_InternalDependency), pointer :: SUT_withMockedMethod     

     !print *,9000
     
     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method4', &
          &   argumentsEqual(-1)))

     ! SUT_withMockedMethod = newMockSUT_InternalDependency()
     !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
     SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
     
     call SUT_withMockedMethod%method4(999)

     !? ! If an exception is caught, end NOW! Need to add some flow control here.
     !? call assertTrue(catch(NULL_MESSAGE)) ! For example...
     !? ! call catchAndQuit ! Intermediate
     !? if (anyExceptions()) return

     call SUT_withMockedMethod%method4(-1)  ! Should this even be called?  If we're backing out?

     call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
     call MockRepositoryPointer%verify() ! Wrap up at end of tests...

     call assertCatch(&
          &  'MockSUT_InternalDependency%method4" "argumentsEqual"'//&
          &  ' does not hold: expected: <-1> but found: <999>')

   end subroutine testArgumentConstraint2

   subroutine testArgumentConstraint3()

     !class (SUT_InternalDependency), allocatable :: SUT_withMockedMethod
     class (SUT_InternalDependency), pointer :: SUT_withMockedMethod

     !print *,10000
     
     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          & 'MockSUT_InternalDependency%method5', &
          & argumentsEqual(999.0)))

     ! SUT_withMockedMethod = newMockSUT_InternalDependency()
     !allocate(SUT_withMockedMethod,source=newMockSUT_InternalDependency())
     SUT_withMockedMethod => newMockSUT_InternalDependencyPointer()
     
     call SUT_withMockedMethod%method5(999.0)
     
     call MockRepositoryPointer%enableFinalVerification() ! Test Execution Finished...
     
     call MockRepositoryPointer%verify() ! Wrap up at end of tests...

   end subroutine testArgumentConstraint3
   

!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

  ! Check to see that the test result is as expected...
  subroutine assertCatch(string,location)
    use Params_mod
    use Exception_mod, only: getNumExceptions, Exception, catchNext
    use Assert_mod, only: assertEqual
    character(len=*), intent(in) :: string
    type (SourceLocation), optional, intent(in) :: location
    type (Exception) :: anException

    if (getNumExceptions() > 0) then
       anException = catchNext()

       !, 'exceptions do not match')
       call assertEqual(string,anException%getMessage()) ! ,message='Exception message test')
       if(present(location))then
          call assertEqual( &
               & location%lineNumber,anException%getLineNumber(), &
               & message='Source line number test')
          call assertEqual(location%fileName,anException%getFileName(), &
               & message='Source file name test')
       end if
    else
       !, 'missing exception')
       call assertEqual(string, ' ')
    end if
  end subroutine assertCatch


end module Test_MockRepository_mod
