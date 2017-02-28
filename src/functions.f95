module nt_FunctionsModule
    public :: nt_logistic, logisticd, const, constd

    contains
    
        !Logistic activation function a/(b+exp(-x))
        !args - array of parameter:
        !args(1) - a
        !args(2) - b

        function nt_logistic(x, args) result(fx)
            real, intent(in) :: args(0:)
            real, intent(in) :: x
            real :: fx

            fx = args(1) / (args(2) + exp(-x))

        end function nt_logistic

        !Logistic function derivative
        function logisticd(args, x) result(fx)
            implicit none
            real, intent(in) :: args(:)
            real, intent(in) :: x
            real :: fx

            fx = 2 / (1 + exp(-2*x)) - 1

        end function logisticd

        function const(args, x) result(fx)
            implicit none
            real, intent(in) :: args(:)
            real, intent(in) :: x
            real :: fx

            fx=args(1)

        end function const

        function constd(args, x) result(fx)
            implicit none
            real, intent(in) :: args(:)
            real, intent(in) :: x
            real :: fx

            fx=args(1) / 2

        end function constd

end module nt_FunctionsModule
