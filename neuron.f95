
    subroutine init_Neuron(this,no,id,eta,alpha)
        
        use types
        type(Neuron) :: this
        integer :: i, id,no
        real eta, alpha
        this%id = id
        this%eta = eta
        this%alpha = alpha
        this%no = no -1
        
        allocate(this%outputWeights(0:this%no)) 
        init_loop: do, i=0, this%no
            call random_number(this%outputWeights%weight) 
        end do init_loop
         
    end subroutine init_Neuron

!activation func -----------------------------------------------------
    function activeFunc(x) result(f)
        use types
        real :: x, f
        
        f=1.0/(1.0+exp(-x))

    end function activeFunc

    function activeFuncD(x) result(fd)
        real :: x, fd, f
        
        f=activeFunc(x)
        fd=(1.0-f)*f
                
    end function activeFuncD    
           
!---------------------------------------------------------------------

    
    subroutine adaptWeights(this,prev)
        use types
        type(Neuron) :: this, pn
        type(Layer) :: prev
        integer :: i        
        real oldDeltaWeight, newDeltaWeight
        

        main_loop: do, i=0, prev%n
           pn = prev%neurons(i)
           oldDeltaWeight = pn%outputWeights(this%id)%deltaweight
           newDeltaWeight=this%eta*pn%output*this%gradient+this%alpha*oldDeltaWeight
           pn%outputWeights(this%id)%deltaweight = newDeltaWeight
           pn%outputWeights(this%id)%weight = pn%outputWeights(this%id)%weight + newDeltaWeight  
        end do main_loop
        


    end subroutine adaptWeights



