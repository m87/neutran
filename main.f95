program main
    use types
    implicit none   
    

    integer, dimension(0:2) :: topology
    real, dimension(0:2) :: input
    real, dimension(0:0) :: targets
    integer :: i
    type(Net) :: net1
    topology(0)=3
    topology(1)=2
    topology(2)=1

    input(0)=1.0
    input(1)=0.0
    input(2)=1.0

    targets(0)=1.0

    call init_net(net1,topology,3,0.05,0.1)
    write(*,*) net1%layers(1)%neurons(0)%outputWeights%weight
    do, i=0, 10
        call feedForwardNet(net1, input,3)
        call backProp(net1,targets,1 )
   ! write(*,*) net1%layers(2)%neurons%output
    write(*,*) net1%layers(1)%neurons(0)%outputWeights%weight
    end do

stop

end program main
