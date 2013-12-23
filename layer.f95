module class_layer  

       use types
public::init_layer

contains 
    subroutine init_layer(this,ni,no,eta, alpha)
       use class_neuron
        type(layer) :: this
        integer :: ni,i,no
        real :: eta, alpha
        this%n = ni-1          
        allocate(this%neurons(0:this%n)) 
        init_loop: do, i=0, this%n
            call init_Neuron(this%neurons(i),no,i,eta,alpha) 
        end do init_loop
 
    end subroutine init_layer 

end module class_layer
