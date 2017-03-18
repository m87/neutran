module nt_NetModule
    use nt_TypesModule
    use nt_LayerModule
    use nt_FunctionsModule
    use nt_InitMethodsModule

    public :: nt_netInit, nt_train, nt_test, nt_classify, nt_trainAll

    interface nt_netInit
        module procedure nt_netInit_default, nt_netInit_custom
    end interface nt_netInit

    interface nt_train
        module procedure nt_train_default, nt_train_custom
    end interface

    interface nt_trainAll
        module procedure nt_trainAll_default, nt_trainAll_custom
    end interface

    contains

        subroutine nt_netInit_default(this, topology)
            type(nt_Net) :: this
            integer, dimension(0:) :: topology
               
            this%topology = topology
            allocate(this%layers(0:size(topology)))
            call nt_layerInit(this%layers(0), 0, topology(0), topology(1), id, .TRUE.)
            
            init_loop: do, i=1, size(topology) - 1
                call nt_layerInit(this%layers(i), 1, topology(i), topology(i+1), id, .TRUE.)
            end do init_loop
            
            call nt_layerInit(this%layers(size(topology)-1), 1, topology(size(topology)-1), 0, size(topology)-1, .TRUE.)

        end subroutine nt_netInit_default

        subroutine nt_netInit_custom(this, topology, bias, weightInitMethod, weightInitMethodArgs)
            type(nt_Net) :: this
            integer, dimension(0:) :: topology
            logical :: bias
            real, intent(in) :: weightInitMethodArgs(:)

            interface 
                subroutine weightInitMethod(weight, args)
                    real, intent(out) :: weight
                    real, intent(in) :: args(0:)
                end subroutine weightInitMethod
            end interface              

            this%topology = topology
            allocate(this%layers(0:size(topology)-1))
            call nt_layerInit(this%layers(0), 0, topology(0), topology(1), 0, weightInitMethod, weightInitMethodArgs, bias)
            
            init_loop: do, i=1, size(topology) - 2
                call nt_layerInit(this%layers(i), 1, topology(i), topology(i+1), i, weightInitMethod, weightInitMethodArgs, bias)
            end do init_loop
            
            call nt_layerInit(this%layers(size(topology)-1), 1, topology(size(topology)-1), size(topology)-1, size(topology)-1, & 
                weightInitMethod, weightInitMethodArgs, bias)


        end subroutine

        subroutine nt_train_default(this)
            type(nt_net) :: this
        end subroutine nt_train_default 
        
        subroutine nt_train_custom(this, dummy)
            type(nt_net) :: this, dummy
        end subroutine nt_train_custom

        subroutine nt_trainAll_default(this)
            type(nt_Net) :: this
        end subroutine nt_trainAll_default

        subroutine nt_trainAll_custom(this, dummy)
            type(nt_Net) :: this, dummy
        end subroutine nt_trainAll_custom

        subroutine nt_test(this)
            type(nt_Net) :: this
        end subroutine nt_test

        subroutine nt_classify(this)
            type(nt_Net) :: this
        end subroutine nt_classify    

        subroutine nt_outputGradients(this, targets, activationFunctionDerivative, args)
            type(nt_Net) :: this
            real :: targets(0:)
            real :: args(0:)

            interface
                function activationFunctionDerivative(x, args) result(fx)
                    real, intent(in) :: x
                    real, intent(in) :: args(0:)
                    real :: fx
                end function activationFunctionDerivative
            end interface

        end subroutine nt_outputGradients

        subroutine nt_hiddenGradients(this, targets)
            type(nt_Net) :: this
            real :: targets(0:)

            interface
                function activationFunctionDerivative(x, args) result(fx)
                    real, intent(in) :: x
                    real, intent(in) :: args(0:)
                    real :: fx
                end function activationFunctionDerivative
            end interface


        end subroutine nt_hiddenGradients

        subroutine nt_adaptNeurons(this)
            type(nt_Net) :: this

        end subroutine nt_adaptNeurons

        subroutine nt_netFeed(this, activationFunction, args, input)
            type(nt_Net) :: this
            real :: args(0:)
            real :: input(0:)
            integer :: i, l
            integer :: numberOfLayers
            
            interface
                function activationFunction(x, args) result(fx)
                    real, intent(in) :: x
                    real, intent(in) :: args(0:)
                    real :: fx
                end function activationFunction
            end interface

            numberOfLayers = size(this%topology)

            call nt_layerFeedInput(this%layers(0), input)

            do, i=1, numberOfLayers-1
                call nt_layerFeed(this%layers(i-1), this%layers(i), activationFunction, args)
            end do
               
        end subroutine nt_netFeed

        subroutine nt_netBackPropagation(this, targets, activationFunction, activationFunctionDerivative, args)
            type(nt_Net) :: this
            real :: args(0:)
            real, intent(in) :: targets(0:)
    
            interface
                function activationFunction(x, args) result(fx)
                    real :: x
                    real :: args(0:)
                    real :: fx
                end function activationFunction
                
                function activationFunctionDerivative(x, args) result(fx)
                    real :: x
                    real :: args(0:)
                    real :: fx
                end function activationFunctionDerivative

            end interface

        end subroutine nt_netBackPropagation

end module nt_NetModule
