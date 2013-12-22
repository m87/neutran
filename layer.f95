   
    subroutine init_layer(this,n)
        use types
        type(layer) :: this
        integer :: n,i
        this%n = n-1          
        
        allocate(this%neurons(0:this%n)) 
        init_loop: do, i=0, this%n
            call init_Neuron(this%neurons(i),1,i,0.0,0.0) 
        end do init_loop
 
    end subroutine init_layer 


