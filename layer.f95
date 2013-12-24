module class_layer  

       use types
public::layer_init

contains 
    subroutine layer_init(this,ni,no,eta, alpha)
       use class_neuron
        type(layer) :: this
        integer :: ni,i,no
        real :: eta, alpha
        this%n = ni-1          
        allocate(this%neurons(0:this%n)) 
        init_loop: do, i=0, this%n
            call neuron_init(this%neurons(i),no,i,eta,alpha) 
        end do init_loop
 
    end subroutine layer_init 

end module class_layer
