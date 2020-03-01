
  !!!!!!!!!!!!!!!!! cartesian_distance !!!!!!!!!!!!!!!!!

  subroutine cartesian_distance(xyp, xym, n, dst)
    implicit none
    integer, parameter :: rp = selected_real_kind(15)
    integer, intent(in) :: n
    real(rp), intent(in) :: xyp(2), xym(n, 2)
    real(rp), intent(out) :: dst(n)

    dst = sqrt((xyp(1) - xym(:, 1))**2 + (xyp(2) - xym(:, 2))**2)
  end subroutine cartesian_distance

  !!!!!!!!!!!!!!!!! cos_spheric_distance !!!!!!!!!!!!!!!!!

  subroutine cos_spheric_distance(xyp, xym, n, dst)
    implicit none
    integer, parameter :: rp = selected_real_kind(15)
    integer, intent(in) :: n
    real(rp), intent(in) :: xyp(2), xym(n, 2)
    real(rp), intent(out) :: dst(n)
    real(rp) :: sin_yp, cos_yp, const
    real(rp) :: sin_ym(n), cos_ym(n), d_cos_xpm(n), sin_y(n), cos_y(n)
    real(rp), parameter :: pi = 4.0_rp * atan(1.0_rp)

    const = pi/180.0_rp
    sin_yp = sin(xyp(2) * const)
    sin_ym = sin(xym(:, 2) * const)
    cos_yp = cos(xyp(2) * const)
    cos_ym = cos(xym(:, 2) * const)
    d_cos_xpm = cos((xyp(1) - xym(:, 1)) * const)
    sin_y = sin_yp * sin_ym
    cos_y = cos_yp * cos_ym
    dst = sin_y + cos_y * d_cos_xpm
  end subroutine cos_spheric_distance

  !!!!!!!!!!!!!!!!! spheric_distance !!!!!!!!!!!!!!!!!

  subroutine spheric_distance(xyp, xym, n, dst)
    implicit none
    integer, parameter :: rp = selected_real_kind(15)
    integer, intent(in) :: n
    real(rp), intent(in) :: xyp(2), xym(n, 2)
    real(rp), intent(out) :: dst(n)
    real(rp) :: res(n)

    call cos_spheric_distance(xyp, xym, n, res)
    dst =  acos(res)
  end subroutine spheric_distance

  !!!!!!!!!!!!!!!!! distance_vector !!!!!!!!!!!!!!!!!

  subroutine distance_vector(xyp, xym, n, spheric, dst)
    implicit none
    integer, parameter :: rp = selected_real_kind(15)
    integer, intent(in) :: n
    integer, intent(in) :: spheric
    real(rp), intent(in) :: xyp(2), xym(n, 2)
    real(rp), intent(out) :: dst(n)

    if (spheric == 1) then
        call spheric_distance(xyp, xym, n, dst)
    else
        call cartesian_distance(xyp, xym, n, dst)
    end if
  end subroutine distance_vector

  !!!!!!!!!!!!!!!!! distance_matrix !!!!!!!!!!!!!!!!!

  subroutine distance_matrix(xym1, xym2, n1, n2, spheric, dst)
    implicit none
    integer, parameter :: rp = selected_real_kind(15)
    integer, intent(in) :: n1, n2
    integer, intent(in) :: spheric
    real(rp), intent(in) :: xym1(n1, 2), xym2(n2, 2)
    real(rp), intent(out) :: dst(n2, n1)
    real(rp) :: res(n2)
    integer :: i

    do i = 1, n1
        call distance_vector(xym1(i, :), xym2, n2, spheric, res)
        dst(:, i) = res
    end do
  end subroutine distance_matrix

  !!!!!!!!!!!!!!!!! distance_pixel !!!!!!!!!!!!!!!!!
  ! Variable radius of influence

  subroutine distance_pixel(xyp, xym, n, nmin, nmax, spheric, dst, rmax, npts, order)
    implicit none
    integer, parameter :: rp = selected_real_kind(15)
    integer, intent(in) :: n, nmin, nmax
    integer, intent(in) :: spheric
    real(rp), intent(in) :: xyp(2), xym(n, 2)
    real(rp), intent(out) :: dst(n), rmax
    integer, intent(out) :: order(n), npts
    real(rp) :: rsconst
    integer :: nmean, npl(n)

    call distance_vector(xyp, xym, n, spheric, dst)
    call order_vector(dst, n, order)

    nmean = int((nmin + nmax)/2)
    if(nmean > n) nmean = n
    rsconst = sum(dst(1:nmean))/nmean

    where(dst <= rsconst)
      npl = 1
    elsewhere
      npl = 0
    endwhere
    npts = sum(npl)

    if(npts < nmin) then
      rmax = dst(nmin)
    else if(npts > nmax) then
      rmax = dst(nmax)
    else
      rmax = rsconst
    end if

    where(dst <= rmax)
      npl = 1
    elsewhere
      npl = 0
    endwhere
    npts = sum(npl)
  end subroutine distance_pixel

  !!!!!!!!!!!!!!!!! distance_pixel !!!!!!!!!!!!!!!!!
  ! Fixed radius of influence

  ! subroutine distance_pixel_fixed(xyp, xym, n, nmin, nmax, spheric, dst, rmax, npts, order)
  !   implicit none
  !   integer, parameter :: rp = selected_real_kind(15)
  !   integer, intent(in) :: n, nmin, nmax
  !   integer, intent(in) :: spheric
  !   real(rp), intent(in) :: xyp(2), xym(n, 2)
  !   real(rp), intent(inout) :: rmax
  !   real(rp), intent(out) :: dst(n)
  !   integer, intent(out) :: order(n), npts
  !   real(rp) :: rsconst
  !   integer :: nmean, npl(n)

  !   call distance_vector(xyp, xym, n, spheric, dst)
  !   call order_vector(dst, n, order)

  !   ! nmean = int((nmin + nmax)/2)
  !   ! if(nmean > n) nmean = n
  !   ! rsconst = sum(dst(1:nmean))/nmean

  !   where(dst <= rmax)
  !     npl = 1
  !   elsewhere
  !     npl = 0
  !   endwhere
  !   npts = sum(npl)

  !   ! if(npts < nmin) then
  !   !   rmax = dst(nmin)
  !   ! else if(npts > nmax) then
  !   !   rmax = dst(nmax)
  !   ! else
  !   !   rmax = rsconst
  !   ! end if

  !   ! where(dst <= rmax)
  !   !   npl = 1
  !   ! elsewhere
  !   !   npl = 0
  !   ! endwhere
  !   ! npts = sum(npl)
  ! end subroutine distance_pixel_fixed

  !!!!!!!!!!!!!!!!! order small array !!!!!!!!!!!!!!!!!

  subroutine order_vector(vec, n, order)
    implicit none
    integer, parameter :: rp = selected_real_kind(15)
    integer, intent(in) :: n
    real(rp), intent(inout) :: vec(n)
    integer, intent(out) :: order(n)
    integer :: i, k, m
    real(rp) :: start, pivot
    integer :: ns, np

    order = (/ (i, i = 1, n) /)

    do i = 1, n - 1
      start = vec(i)
      ns = order(i)
      m = i + 1
      do k = m, n
        if (start <= vec(k)) cycle
        pivot = start; start = vec(k); vec(k) = pivot
        np = ns; ns = order(k); order(k) = np
      end do
      vec(i) = start
      order(i) = ns
    end do
  end subroutine order_vector
