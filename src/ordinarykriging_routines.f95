!!!!!!!!!!!!!!!!! Ordinary kriging !!!!!!!!!!!!!!!!!

subroutine kriging_interp(xyStn, valStn, nStn, xyGrd, nGrd, nmin, nmax, spheric,&
                          vgm, nug, sill, rg, outM)
  implicit none
  integer, parameter :: rp = selected_real_kind(15)
  integer, intent(in) :: nStn, nGrd, nmin, nmax
  integer, intent(in) :: spheric
  real(rp), intent(in) :: xyStn(nStn, 2), valStn(nStn), xyGrd(nGrd, 2)
  integer, intent(in) :: vgm
  real(rp), intent(in) :: nug, sill, rg
  real(rp), intent(out) :: outM(nGrd, 4)
  real(rp) :: out(2), outNaN(2), Fillval, x
  integer :: i, ret

  x = -1.0_rp
  Fillval = sqrt(x)
  outNaN(1:2) = Fillval
  outM(:, 1:2) = xyGrd
  do i = 1, nGrd
    call okriging_pixel(xyGrd(i, :), xyStn, valStn, nStn, nmin, nmax, spheric,&
                        vgm, nug, sill, rg, out, ret)
    if(ret == 0) then
      outM(i, 3:4) = out
    else
      outM(i, 3:4) = outNaN
    end if
  end do
end subroutine kriging_interp

subroutine okriging_pixel(xyp, xym, values, n, nmin, nmax, spheric,&
                          vgm, nug, sill, rg, out, ret)
  implicit none
  integer, parameter :: rp = selected_real_kind(15)
  integer, intent(in) :: n, nmin, nmax
  integer, intent(in) :: spheric
  real(rp), intent(in) :: xyp(2), xym(n, 2), values(n)
  integer, intent(in) :: vgm
  real(rp), intent(in) :: nug, sill, rg
  real(rp), intent(out) :: out(2)
  integer, intent(out) :: ret
  real(rp) :: dst(n), rmax
  integer :: order(n), npts, npts1
  real(rp) :: zs0(n), xys0(n, 2)
  real(rp), dimension(:), allocatable :: zs, xdst, dG, vG, Wk
  real(rp), dimension(:, :), allocatable :: xys1, dS, vS, predm, mdG, vG1, predv

  call distance_pixel(xyp, xym, n, nmin, nmax, spheric, dst, rmax, npts, order)

  npts1 = npts + 1
  allocate(zs(npts), xdst(npts))

  zs0 = values(order)
  zs = zs0(1:npts)
  xdst = dst(1:npts)

  if(all(xdst < 1.e-10)) then
    out(1) = sum(zs)/npts
    out(2) = 0
    deallocate(zs, xdst)
    return
  end if

  if(abs(maxval(zs) - minval(zs)) < 1.e-10) then
    out(1) = zs(1)
    out(2) = 0
    deallocate(zs, xdst)
    return
  end if

  allocate(xys1(npts, 2), dS(npts, npts), dG(npts), mdG(npts, 1))
  allocate(predm(npts, npts), vS(npts1, npts1), vG1(npts1, 1))
  allocate(predv(npts, 1), vG(npts1), Wk(npts1))

  xys0 = xym(order, :)
  xys1 = xys0(1:npts, :)

  call distance_matrix(xys1, xys1, npts, npts, spheric, dS)
  call distance_vector(xyp, xys1, npts, spheric, dG)
  call predict_vgm(dS, npts, npts, vgm, nug, sill, rg, predm)
  vS(1:npts, 1:npts) = sill - predm
  vS(:, npts1) = 1.0_rp
  vS(npts1, :) = 1.0_rp
  vS(npts1, npts1) = 0.0_rp
  
  mdG(1:npts, 1) = dG
  call predict_vgm(mdG, npts, 1, vgm, nug, sill, rg, predv)
  vG(1:npts) = sill - predv(:, 1)
  vG(npts1) = 1.0_rp

  vG1(:, 1) = vG
  call dsysv_solve(vS, vG1, npts1, 1, ret)
  if(ret /= 0) return
  Wk = vG1(:, 1)

  out(1) = sum(zs * Wk(1:npts))
  out(2) = sill - sum(Wk * vG)
  deallocate(zs, xdst, xys1, dS, dG, mdG, predm, vS, predv, vG, vG1, Wk)
end subroutine okriging_pixel

subroutine predict_vgm(x, n, m, vgm, nug, sill, rg, pred)
  implicit none
  integer, parameter :: rp = selected_real_kind(15)
  integer, intent(in) :: n, m
  integer, intent(in) :: vgm
  real(rp), intent(in) :: x(n, m), nug, sill, rg
  real(rp), intent(out) :: pred(n, m)

  if(vgm == 1) then
    ! "Gau"
    pred = nug + (sill - nug) * (1.0_rp - exp(-3.0_rp * (x/rg)**2))
  else if(vgm == 2) then
    ! "Exp"
    pred = nug + (sill - nug) * (1.0_rp - exp(-3.0_rp * x/rg))
  else if(vgm == 3) then
    ! "Sph"
    where(x <= rg)
      pred = nug + (sill - nug) * (1.5_rp * (x/rg) - 0.5_rp * (x/rg)**3)
    elsewhere
      pred = sill
    end where
  else if(vgm == 4) then
    ! "Pen"
    where(x <= rg)
      pred = nug + (sill - nug) * (1.875_rp * (x/rg) - 1.25_rp * (x/rg)**3 + 0.375_rp * (x/rg)**5)
    elsewhere
      pred = sill
    end where
  else
    stop 1
  end if
end subroutine predict_vgm
