module Functions
    public :: logistic, logisticd, const, constd

    contains

        function logistic(args, x) result(fx)
            implicit none
            real, intent(in) :: args(:)
            real, intent(in) :: x
            real :: fx

            fx=0

        end function logistic

        function logisticd(args, x) result(fx)
            implicit none
            real, intent(in) :: args(:)
            real, intent(in) :: x
            real :: fx

            fx=0

        end function logisticd

        function const(args, x) result(fx)
            implicit none
            real, intent(in) :: args(:)
            real, intent(in) :: x
            real :: fx

            fx=0

        end function const

        function constd(args, x) result(fx)
            implicit none
            real, intent(in) :: args(:)
            real, intent(in) :: x
            real :: fx

            fx=0

        end function constd

end module Functions
