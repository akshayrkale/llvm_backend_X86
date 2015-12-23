!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!     
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
      subroutine extrapolate(xi,xn,ipkon,inum,kon,lakon,nfield,nk,
     &  ne,mint_,ndim)
!
!     extrapolates field values at the integration points to the 
!     nodes
!
!     the number of internal state variables is limited to 999
!     (cfr. array field)
!
      implicit none
!
      character*8 lakon(*),lakonl
!
      integer ipkon(*),inum(*),kon(*),mint_,ne,indexe,nope,
     &  nonei20(3,12),nfield,nonei10(3,6),nk,i,j,k,l,ndim,
     &  nonei15(3,9)
!
      real*8 xi(ndim,mint_,*),xn(nfield,*),field(999,20),a8(8,8),
     &  a4(4,4),a27(20,27),a9(6,9),a2(6,2)
!
      data nonei10 /5,1,2,6,2,3,7,3,1,8,1,4,9,2,4,10,3,4/
!
      data nonei15 /7,1,2,8,2,3,9,3,1,10,4,5,11,5,6,12,6,4,
     &  13,1,4,14,2,5,15,3,6/
!
      data nonei20 /9,1,2,10,2,3,11,3,4,12,4,1,
     &  13,5,6,14,6,7,15,7,8,16,8,5,
     &  17,1,5,18,2,6,19,3,7,20,4,8/
!
      data a2 /  1.1455,-0.1455,1.1455,-0.1455,1.1455,-0.1455,
     &           -0.1455,1.1455,-0.1455,1.1455,-0.1455,1.1455/
      data a4 /  1.92705, -0.30902, -0.30902, -0.30902,
     &          -0.30902,  1.92705, -0.30902, -0.30902,
     &          -0.30902, -0.30902,  1.92705, -0.30902,
     &          -0.30902, -0.30902, -0.30902,  1.92705/
      data a9 / 1.63138,-0.32628,-0.32628,-0.52027, 0.10405, 0.10405,
     &         -0.32628, 1.63138,-0.32628, 0.10405,-0.52027, 0.10405,
     &         -0.32628,-0.32628, 1.63138, 0.10405, 0.10405,-0.52027,
     &          0.55556,-0.11111,-0.11111, 0.55556,-0.11111,-0.11111,
     &         -0.11111, 0.55556,-0.11111,-0.11111, 0.55556,-0.11111,
     &         -0.11111,-0.11111, 0.55556,-0.11111,-0.11111, 0.55556,
     &         -0.52027, 0.10405, 0.10405, 1.63138,-0.32628,-0.32628,
     &          0.10405,-0.52027, 0.10405,-0.32628, 1.63138,-0.32628,
     &          0.10405, 0.10405,-0.52027,-0.32628,-0.32628, 1.63138/
      data a8 /2.549,-.683,.183,-.683,-.683,.183,
     &        -.04904,.183,-.683,2.549,-.683,.183,
     &        .183,-.683,.183,-.04904,-.683,.183,
     &        -.683,2.549,.183,-.04904,.183,-.683,
     &        .183,-.683,2.549,-.683,-.04904,.183,
     &        -.683,.183,-.683,.183,-.04904,.183,
     &        2.549,-.683,.183,-.683,.183,-.683,
     &        .183,-.04904,-.683,2.549,-.683,.183,
     &        .183,-.04904,.183,-.683,-.683,.183,
     &        -.683,2.549,-.04904,.183,-.683,.183,
     &        .183,-.683,2.549,-.683/      
      data a27 /
     &  2.37499,-0.12559,-0.16145,-0.12559,-0.12559,-0.16145, 0.11575,
     & -0.16145, 0.32628, 0.11111, 0.11111, 0.32628, 0.11111,-0.10405,
     & -0.10405, 0.11111, 0.32628, 0.11111,-0.10405, 0.11111,-0.31246,
     & -0.31246, 0.31481, 0.31481, 0.31481, 0.31481,-0.16902,-0.16902,
     &  1.28439,-0.27072,-0.19444,-0.27072,-0.19444, 0.15961,-0.00661,
     &  0.15961,-0.27072,-0.27072, 0.15961, 0.15961,-0.12559, 2.37499,
     & -0.12559,-0.16145,-0.16145,-0.12559,-0.16145, 0.11575, 0.32628,
     &  0.32628, 0.11111, 0.11111, 0.11111, 0.11111,-0.10405,-0.10405,
     &  0.11111, 0.32628, 0.11111,-0.10405,-0.31246, 0.31481, 0.31481,
     & -0.31246, 0.31481,-0.16902,-0.16902, 0.31481,-0.27072,-0.19444,
     & -0.27072, 1.28439, 0.15961,-0.00661, 0.15961,-0.19444,-0.27072,
     &  0.15961, 0.15961,-0.27072,-0.48824,-0.48824,-0.48824,-0.48824,
     &  0.22898, 0.22898, 0.22898, 0.22898, 0.05556, 0.05556, 0.05556,
     &  0.05556, 0.05556, 0.05556, 0.05556, 0.05556,-0.22222,-0.22222,
     & -0.22222,-0.22222, 0.31481,-0.31246,-0.31246, 0.31481,-0.16902,
     &  0.31481, 0.31481,-0.16902,-0.27072, 1.28439,-0.27072,-0.19444,
     &  0.15961,-0.19444, 0.15961,-0.00661, 0.15961,-0.27072,-0.27072,
     &  0.15961,-0.12559,-0.16145,-0.12559, 2.37499,-0.16145, 0.11575,
     & -0.16145,-0.12559, 0.11111, 0.11111, 0.32628, 0.32628,-0.10405,
     & -0.10405, 0.11111, 0.11111, 0.11111,-0.10405, 0.11111, 0.32628,
     &  0.31481, 0.31481,-0.31246,-0.31246,-0.16902,-0.16902, 0.31481,
     &  0.31481,-0.19444,-0.27072, 1.28439,-0.27072,-0.00661, 0.15961,
     & -0.19444, 0.15961, 0.15961, 0.15961,-0.27072,-0.27072,-0.16145,
     & -0.12559, 2.37499,-0.12559, 0.11575,-0.16145,-0.12559,-0.16145,
     &  0.11111, 0.32628, 0.32628, 0.11111,-0.10405, 0.11111, 0.11111,
     & -0.10405,-0.10405, 0.11111, 0.32628, 0.11111,-0.31246, 0.31481,
     & -0.16902, 0.31481,-0.31246, 0.31481,-0.16902, 0.31481,-0.27072,
     &  0.15961, 0.15961,-0.27072,-0.27072, 0.15961, 0.15961,-0.27072,
     &  1.28439,-0.19444,-0.00661,-0.19444,-0.48824,-0.48824, 0.22898,
     &  0.22898,-0.48824,-0.48824, 0.22898, 0.22898, 0.05556,-0.22222,
     &  0.05556,-0.22222, 0.05556,-0.22222, 0.05556,-0.22222, 0.05556,
     &  0.05556, 0.05556, 0.05556, 0.31481,-0.31246, 0.31481,-0.16902,
     &  0.31481,-0.31246, 0.31481,-0.16902,-0.27072,-0.27072, 0.15961,
     &  0.15961,-0.27072,-0.27072, 0.15961, 0.15961,-0.19444, 1.28439,
     & -0.19444,-0.00661,-0.48824, 0.22898, 0.22898,-0.48824,-0.48824,
     &  0.22898, 0.22898,-0.48824,-0.22222, 0.05556,-0.22222, 0.05556,
     & -0.22222, 0.05556,-0.22222, 0.05556, 0.05556, 0.05556, 0.05556,
     &  0.05556,-0.29630,-0.29630,-0.29630,-0.29630,-0.29630,-0.29630,
     & -0.29630,-0.29630,-0.11111,-0.11111,-0.11111,-0.11111,-0.11111,
     & -0.11111,-0.11111,-0.11111,-0.11111,-0.11111,-0.11111,-0.11111,
     &  0.22898,-0.48824,-0.48824, 0.22898, 0.22898,-0.48824,-0.48824,
     &  0.22898,-0.22222, 0.05556,-0.22222, 0.05556,-0.22222, 0.05556,
     & -0.22222, 0.05556, 0.05556, 0.05556, 0.05556, 0.05556, 0.31481,
     & -0.16902, 0.31481,-0.31246, 0.31481,-0.16902, 0.31481,-0.31246,
     &  0.15961, 0.15961,-0.27072,-0.27072, 0.15961, 0.15961,-0.27072,
     & -0.27072,-0.19444,-0.00661,-0.19444, 1.28439, 0.22898, 0.22898,
     & -0.48824,-0.48824, 0.22898, 0.22898,-0.48824,-0.48824, 0.05556,
     & -0.22222, 0.05556,-0.22222, 0.05556,-0.22222, 0.05556,-0.22222,
     &  0.05556, 0.05556, 0.05556, 0.05556,-0.16902, 0.31481,-0.31246,
     &  0.31481,-0.16902, 0.31481,-0.31246, 0.31481, 0.15961,-0.27072,
     & -0.27072, 0.15961, 0.15961,-0.27072,-0.27072, 0.15961,-0.00661,
     & -0.19444, 1.28439,-0.19444,-0.12559,-0.16145, 0.11575,-0.16145,
     &  2.37499,-0.12559,-0.16145,-0.12559, 0.11111,-0.10405,-0.10405,
     &  0.11111, 0.32628, 0.11111, 0.11111, 0.32628, 0.32628, 0.11111,
     & -0.10405, 0.11111, 0.31481, 0.31481,-0.16902,-0.16902,-0.31246,
     & -0.31246, 0.31481, 0.31481,-0.19444, 0.15961,-0.00661, 0.15961,
     &  1.28439,-0.27072,-0.19444,-0.27072,-0.27072,-0.27072, 0.15961,
     &  0.15961,-0.16145,-0.12559,-0.16145, 0.11575,-0.12559, 2.37499,
     & -0.12559,-0.16145, 0.11111, 0.11111,-0.10405,-0.10405, 0.32628,
     &  0.32628, 0.11111, 0.11111, 0.11111, 0.32628, 0.11111,-0.10405,
     &  0.31481,-0.16902,-0.16902, 0.31481,-0.31246, 0.31481, 0.31481,
     & -0.31246, 0.15961,-0.00661, 0.15961,-0.19444,-0.27072,-0.19444,
     & -0.27072, 1.28439,-0.27072, 0.15961, 0.15961,-0.27072, 0.22898,
     &  0.22898, 0.22898, 0.22898,-0.48824,-0.48824,-0.48824,-0.48824,
     &  0.05556, 0.05556, 0.05556, 0.05556, 0.05556, 0.05556, 0.05556,
     &  0.05556,-0.22222,-0.22222,-0.22222,-0.22222,-0.16902, 0.31481,
     &  0.31481,-0.16902, 0.31481,-0.31246,-0.31246, 0.31481, 0.15961,
     & -0.19444, 0.15961,-0.00661,-0.27072, 1.28439,-0.27072,-0.19444,
     &  0.15961,-0.27072,-0.27072, 0.15961,-0.16145, 0.11575,-0.16145,
     & -0.12559,-0.12559,-0.16145,-0.12559, 2.37499,-0.10405,-0.10405,
     &  0.11111, 0.11111, 0.11111, 0.11111, 0.32628, 0.32628, 0.11111,
     & -0.10405, 0.11111, 0.32628,-0.16902,-0.16902, 0.31481, 0.31481,
     &  0.31481, 0.31481,-0.31246,-0.31246,-0.00661, 0.15961,-0.19444,
     &  0.15961,-0.19444,-0.27072, 1.28439,-0.27072, 0.15961, 0.15961,
     & -0.27072,-0.27072, 0.11575,-0.16145,-0.12559,-0.16145,-0.16145,
     & -0.12559, 2.37499,-0.12559,-0.10405, 0.11111, 0.11111,-0.10405,
     &  0.11111, 0.32628, 0.32628, 0.11111,-0.10405, 0.11111, 0.32628,
     &  0.11111/
c     &  3.23411, 0.41079, 0.05218, 0.41079, 0.41079, 0.05218, 0.00663,
c     &  0.05218, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -1.45796,-1.45796,-0.18519,-0.18519,-0.18519,-0.18519,-0.02352,
c     & -0.02352, 2.18694, 0.00000, 0.27778, 0.00000, 0.27778, 0.00000,
c     &  0.03528, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.41079, 3.23411, 0.41079, 0.05218, 0.05218, 0.41079, 0.05218,
c     &  0.00663, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -1.45796,-0.18519,-0.18519,-1.45796,-0.18519,-0.02352,-0.02352,
c     & -0.18519, 0.00000, 0.27778, 0.00000, 2.18694, 0.00000, 0.03528,
c     &  0.00000, 0.27778, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.65726, 0.65726, 0.65726, 0.65726, 0.08348, 0.08348, 0.08348,
c     &  0.08348,-0.98589,-0.98589,-0.98589,-0.98589,-0.12522,-0.12522,
c     & -0.12522,-0.12522, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -0.18519,-1.45796,-1.45796,-0.18519,-0.02352,-0.18519,-0.18519,
c     & -0.02352, 0.00000, 2.18694, 0.00000, 0.27778, 0.00000, 0.27778,
c     &  0.00000, 0.03528, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.41079, 0.05218, 0.41079, 3.23411, 0.05218, 0.00663, 0.05218,
c     &  0.41079, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -0.18519,-0.18519,-1.45796,-1.45796,-0.02352,-0.02352,-0.18519,
c     & -0.18519, 0.27778, 0.00000, 2.18694, 0.00000, 0.03528, 0.00000,
c     &  0.27778, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.05218, 0.41079, 3.23411, 0.41079, 0.00663, 0.05218, 0.41079,
c     &  0.05218, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -1.45796,-0.18519,-0.02352,-0.18519,-1.45796,-0.18519,-0.02352,
c     & -0.18519, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 2.18694, 0.27778, 0.03528, 0.27778,
c     &  0.65726, 0.65726, 0.08348, 0.08348, 0.65726, 0.65726, 0.08348,
c     &  0.08348,-0.98589, 0.00000,-0.12522, 0.00000,-0.98589, 0.00000,
c     & -0.12522, 0.00000,-0.98589,-0.98589,-0.12522,-0.12522,
c     & -0.18519,-1.45796,-0.18519,-0.02352,-0.18519,-1.45796,-0.18519,
c     & -0.02352, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.27778, 2.18694, 0.27778, 0.03528,
c     &  0.65726, 0.08348, 0.08348, 0.65726, 0.65726, 0.08348, 0.08348,
c     &  0.65726, 0.00000,-0.12522, 0.00000,-0.98589, 0.00000,-0.12522,
c     &  0.00000,-0.98589,-0.98589,-0.12522,-0.12522,-0.98589,
c     & -0.29630,-0.29630,-0.29630,-0.29630,-0.29630,-0.29630,-0.29630,
c     & -0.29630, 0.44444, 0.44444, 0.44444, 0.44444, 0.44444, 0.44444,
c     &  0.44444, 0.44444, 0.44444, 0.44444, 0.44444, 0.44444,
c     &  0.08348, 0.65726, 0.65726, 0.08348, 0.08348, 0.65726, 0.65726,
c     &  0.08348, 0.00000,-0.98589, 0.00000,-0.12522, 0.00000,-0.98589,
c     &  0.00000,-0.12522,-0.12522,-0.98589,-0.98589,-0.12522,
c     & -0.18519,-0.02352,-0.18519,-1.45796,-0.18519,-0.02352,-0.18519,
c     & -1.45796, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.27778, 0.03528, 0.27778, 2.18694,
c     &  0.08348, 0.08348, 0.65726, 0.65726, 0.08348, 0.08348, 0.65726,
c     &  0.65726,-0.12522, 0.00000,-0.98589, 0.00000,-0.12522, 0.00000,
c     & -0.98589, 0.00000,-0.12522,-0.12522,-0.98589,-0.98589,
c     & -0.02352,-0.18519,-1.45796,-0.18519,-0.02352,-0.18519,-1.45796,
c     & -0.18519, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.03528, 0.27778, 2.18694, 0.27778,
c     &  0.41079, 0.05218, 0.00663, 0.05218, 3.23411, 0.41079, 0.05218,
c     &  0.41079, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -0.18519,-0.18519,-0.02352,-0.02352,-1.45796,-1.45796,-0.18519,
c     & -0.18519, 0.27778, 0.00000, 0.03528, 0.00000, 2.18694, 0.00000,
c     &  0.27778, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.05218, 0.41079, 0.05218, 0.00663, 0.41079, 3.23411, 0.41079,
c     &  0.05218, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -0.18519,-0.02352,-0.02352,-0.18519,-1.45796,-0.18519,-0.18519,
c     & -1.45796, 0.00000, 0.03528, 0.00000, 0.27778, 0.00000, 0.27778,
c     &  0.00000, 2.18694, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.08348, 0.08348, 0.08348, 0.08348, 0.65726, 0.65726, 0.65726,
c     &  0.65726,-0.12522,-0.12522,-0.12522,-0.12522,-0.98589,-0.98589,
c     & -0.98589,-0.98589, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -0.02352,-0.18519,-0.18519,-0.02352,-0.18519,-1.45796,-1.45796,
c     & -0.18519, 0.00000, 0.27778, 0.00000, 0.03528, 0.00000, 2.18694,
c     &  0.00000, 0.27778, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.05218, 0.00663, 0.05218, 0.41079, 0.41079, 0.05218, 0.41079,
c     &  3.23411, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     & -0.02352,-0.02352,-0.18519,-0.18519,-0.18519,-0.18519,-1.45796,
c     & -1.45796, 0.03528, 0.00000, 0.27778, 0.00000, 0.27778, 0.00000,
c     &  2.18694, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00663, 0.05218, 0.41079, 0.05218, 0.05218, 0.41079, 3.23411,
c     &  0.41079, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
c     &  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000/
!
      do i=1,nk
         inum(i)=0
      enddo
!
      do i=1,nk
         do j=1,nfield
            xn(j,i)=0.d0
         enddo
      enddo
!
      do i=1,ne
!
         if(ipkon(i).lt.0) cycle
         indexe=ipkon(i)
         lakonl=lakon(i)
!
         if(lakonl(4:4).eq.'2') then
            nope=20
         elseif(lakonl(4:4).eq.'8') then
            nope=8
         elseif(lakonl(4:5).eq.'10') then
            nope=10
         elseif(lakonl(4:4).eq.'4') then
            nope=4
         elseif(lakonl(4:5).eq.'15') then
            nope=15
         else
            nope=6
         endif
!
!        determining the field values in the vertex nodes
!        for C3D20R and C3D8: trilinear extrapolation (= use of the
!                             C3D8 linear interpolation functions)
!        for C3D8R: constant field value in each element
!        for C3D10: use of the C3D4 linear interpolation functions
!        for C3D4: constant field value in each element
!        for C3D15: use of the C3D6 linear interpolation functions
!        for C3D6: use of a linear interpolation function
!
         if((lakonl(4:6).eq.'20R').or.(lakonl(4:5).eq.'8 ')) then
            do j=1,8
               do k=1,nfield
                  field(k,j)=0.d0
                  do l=1,8
                     field(k,j)=field(k,j)+a8(j,l)*xi(k,l,i)
                  enddo
               enddo
            enddo
         elseif(lakonl(4:4).eq.'8') then
            do j=1,8
               do k=1,nfield
                  field(k,j)=xi(k,1,i)
               enddo
            enddo
         elseif(lakonl(4:5).eq.'10') then
            do j=1,4
               do k=1,nfield
                  field(k,j)=0.d0
                  do l=1,4
                     field(k,j)=field(k,j)+a4(j,l)*xi(k,l,i)
                  enddo
               enddo
            enddo
         elseif(lakonl(4:4).eq.'2') then
            do j=1,20
               do k=1,nfield
                  field(k,j)=0.d0
                  do l=1,27
                     field(k,j)=field(k,j)+a27(j,l)*xi(k,l,i)
                  enddo
               enddo
            enddo
         elseif(lakonl(4:4).eq.'4') then
            do j=1,4
               do k=1,nfield
                  field(k,j)=xi(k,1,i)
               enddo
            enddo
         elseif(lakonl(4:4).eq.'1') then
            do j=1,6
               do k=1,nfield
                  field(k,j)=0.d0
                  do l=1,9
                     field(k,j)=field(k,j)+a9(j,l)*xi(k,l,i)
                  enddo
               enddo
            enddo
         else
            do j=1,6
               do k=1,nfield
                  field(k,j)=0.d0
                  do l=1,2
                     field(k,j)=field(k,j)+a2(j,l)*xi(k,l,i)
                  enddo
               enddo
            enddo
         endif
!
!        determining the field values in the midside nodes
!
         if(lakonl(4:4).eq.'2') then
            do j=9,20
               do k=1,nfield
                  field(k,j)=(field(k,nonei20(2,j-8))+
     &                 field(k,nonei20(3,j-8)))/2.d0
               enddo
            enddo
         elseif(lakonl(4:5).eq.'10') then
            do j=5,10
               do k=1,nfield
                  field(k,j)=(field(k,nonei10(2,j-4))+
     &                 field(k,nonei10(3,j-4)))/2.d0
               enddo
            enddo
         elseif(lakonl(4:5).eq.'15') then
            do j=7,15
               do k=1,nfield
                  field(k,j)=(field(k,nonei15(2,j-6))+
     &                 field(k,nonei15(3,j-6)))/2.d0
               enddo
            enddo
         endif
!
!        transferring the field values into xn
!
         do j=1,nope
            do k=1,nfield
               xn(k,kon(indexe+j))=xn(k,kon(indexe+j))+
     &              field(k,j)
            enddo
            inum(kon(indexe+j))=inum(kon(indexe+j))+1
         enddo
!
      enddo
!
!     taking the mean of nodal contributions coming from different
!     elements having the node in common
!
      do i=1,nk
         if(inum(i).gt.0) then
            do j=1,nfield
               xn(j,i)=xn(j,i)/inum(i)
            enddo
         endif
      enddo
!
      return
      end
