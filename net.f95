subroutine init_net(this, topology, n, eta, alpha)
    use types
    type(Net) :: this
    integer,dimension(0:n-1) :: topology
    integer :: n
    real :: eta, alpha
    this%lnum = n-1
        allocate(this%layers(0:this%lnum)) 
        call init_layer(this%layers(this%lnum),topology(this%lnum),0,eta,alpha) 
            
        init_loop: do, i=0, this%lnum-1
            call init_layer(this%layers(i),topology(i),topology(i+1),eta,alpha)
        end do init_loop
 
end subroutine init_net


subroutine backProp(this, targets,n)
    use types
    type(Net) :: this
    type(Layer) :: last
    real, dimension(0:n-1) :: targets
    integer :: i,ln 
    real :: delta

    last = this%layers(this%lnum)
    this%error = 0.0

    do, i=0, last%n
        delta = targets(i) - last%neurons(i)%output
        error = error + delta*delta
    end do

    error = error / last%n
    error = sqrt(error)

    do, i=0, last%n
        call calcOutputGradients(last%neurons(i), targets(i))
    end do

   first: do, ln = this%lnum-1, 1
        second: do,i =0, this%layers(ln)%n
         call calcHiddenGradients(this%layers(ln)%neurons(i), this%layers(ln+1))
        end do second
   end do first

    first1: do, ln = this%lnum , 1
        second1: do, i=0,this%layers(ln)%n
           call adaptWeights(this%layers(ln)%neurons(i), this%layers(ln-1))
        end do second1
    end do first1     

end subroutine backProp

subroutine feedForwardNet(this,input,n)
    use types
    integer :: n
    type(Net) :: this
    real, dimension(0: n-1) :: input
    integer :: i,ln 

         do,i =0, this%layers(0)%n
            this%layers(0)%neurons(i)%output = input(i)
        end do

   first: do, ln = 1, this%lnum
        second: do,i =0, this%layers(ln)%n
            call feedForward(this%layers(ln)%neurons(i), this%layers(ln-1))
        end do second
   end do first

end subroutine feedForwardNet

