MODULE SURFACE_EXTERNAL_COMMON_3D

    USE SURFACE_MODULE_3D
    USE PROPA_RECONST_REINITIAL_3D
    USE OPERATORS_3D
    USE REMESHING_3D

    IMPLICIT NONE

    CONTAINS

    SUBROUTINE READINPUT!(TYP)

	! Local Variables
	INTEGER :: io
	CHARACTER(500) :: cDum
	CHARACTER(500) :: Fname
	LOGICAL :: Exist

	! Find Input file
	Fname = "./input/surface.inp"
	INQUIRE(FILE=Fname, EXIST=Exist)
	IF( .NOT. Exist ) THEN
	  WRITE(*,*) 'surface >> Error : Cannot find input file < surface.inp >'
	  STOP
	END IF

	! Read Input File
	OPEN(newunit=io,File=Fname)
	READ(io,*) 
	READ(io,*)
	READ(io,*) cDum, cDum, RESTART_FLAG
	READ(io,*) cDum, cDum, RESTART_ITERATION 
	READ(io,*)
 	READ(io,*) cDum, cDum, MODEL_ANGLE   
 	READ(io,*) cDum, cDum, MODEL_AXIS_EQN(1,1), MODEL_AXIS_EQN(2,1), MODEL_AXIS_EQN(3,1), MODEL_AXIS_EQN(4,1)   
 	READ(io,*) cDum, cDum, MODEL_AXIS_EQN(1,2), MODEL_AXIS_EQN(2,2), MODEL_AXIS_EQN(3,2), MODEL_AXIS_EQN(4,2) 
   	READ(io,*)
	READ(io,*) cDum, cDum, CHI_C_LOW
	READ(io,*) cDum, cDum, CHI_C_HIGH
	READ(io,*) cDum, cDum, INTERFACE_THRESHOLD
	READ(io,*)
	READ(io,*) cDum, cDum, SMALL_REGION_POINT_NUM
	READ(io,*) cDum, cDum, THIN_REGION_EXISTENCE
	READ(io,*) cDum, cDum, THIN_REGION_ATTACHMENT
	READ(io,*) cDum, cDum, EDGE_SPLITTING
	READ(io,*) cDum, cDum, EDGE_COLLAPSING
	CLOSE(io)

    END SUBROUTINE
    
    SUBROUTINE SAVINGPOINT(FILE_NUM, TYP)
	IMPLICIT NONE
        INTEGER :: TYP
        
        INTEGER :: FILE_NUM
        INTEGER :: I
        CHARACTER(500) :: STR, STR2
        
        INTEGER :: POINT_NUM
        REAL(8), POINTER, DIMENSION(:,:) :: POINT
        
        IF (TYP==0) THEN
            POINT_NUM = SURFACE_FLUID%SURFACE_POINTS_NUM
            POINT => SURFACE_FLUID%SURFACE_POINTS
        END IF
        IF (TYP==1) THEN
            POINT_NUM = SURFACE_PROPEL%SURFACE_POINTS_NUM
            POINT => SURFACE_PROPEL%SURFACE_POINTS
        END IF
        IF (TYP==2) THEN
            POINT_NUM = SURFACE_CASE%SURFACE_POINTS_NUM
            POINT => SURFACE_CASE%SURFACE_POINTS
        END IF
        
        WRITE(STR, *), FILE_NUM
        WRITE(STR2, *), TYP
        STR = './output/surface/pointfaces/point_3d' // TRIM(ADJUSTL(STR)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        WRITE(*,*) TRIM(STR)
    
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        
        !$OMP DO ORDERED 
        DO I = 1, POINT_NUM
        
            WRITE(21,*) POINT(1,I)
            WRITE(21,*) POINT(2,I)
            WRITE(21,*) POINT(3,I)
        
        END DO
        !$OMP END DO
    
        CLOSE(21)
    END SUBROUTINE SAVINGPOINT
    
    SUBROUTINE SAVINGFACE(FILE_NUM, TYP)
	IMPLICIT NONE
        INTEGER :: TYP
        
        INTEGER :: FILE_NUM
        INTEGER :: I
        CHARACTER(500) :: STR, STR2
        
        INTEGER :: FACE_NUM
        INTEGER, POINTER, DIMENSION(:,:) :: FACE
        
        IF (TYP==0) THEN
            FACE_NUM = SURFACE_FLUID%SURFACE_FACES_NUM
            FACE => SURFACE_FLUID%SURFACE_FACES
        END IF
        IF (TYP==1) THEN
            FACE_NUM = SURFACE_PROPEL%SURFACE_FACES_NUM
            FACE => SURFACE_PROPEL%SURFACE_FACES
        END IF
        IF (TYP==2) THEN
            FACE_NUM = SURFACE_CASE%SURFACE_FACES_NUM
            FACE => SURFACE_CASE%SURFACE_FACES
        END IF
        
        WRITE(STR, *), FILE_NUM
        WRITE(STR2, *), TYP
        STR = './output/surface/pointfaces/face_3d' // TRIM(ADJUSTL(STR)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        WRITE(*,*) TRIM(STR)
    
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        
        !$OMP DO ORDERED 
        DO I = 1, FACE_NUM
            WRITE(21,*) FACE(1,I)
            WRITE(21,*) FACE(2,I)
            WRITE(21,*) FACE(3,I)
        END DO
        !$OMP END DO
    
        CLOSE(21)
    END SUBROUTINE SAVINGFACE

    SUBROUTINE SAVINGDATA_TECPLOT(FILE_NUM)
	IMPLICIT NONE
        INTEGER :: FILE_NUM
        INTEGER :: I
        CHARACTER(500) :: STR, STR2, STR3, STR4, STR5, STR6
        INTEGER :: L, L2, L3, L4, L5, L6

        REAL(8) :: R, F_SHAPE, F_SIZE, F_SIZE_SHAPE
    
        INTEGER :: J, J0
        LOGICAL :: B
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        
        STR = ''
        WRITE(STR, '(A)') './output/surface/tec_data_3d/surface_data_0000000.plt'
        L = LEN_TRIM(STR)
        
        STR2 = ''
        WRITE(STR2, '(I)') FILE_NUM
        STR2 = TRIM(ADJUSTL(STR2))
        L2 = LEN_TRIM(STR2)
        
        WRITE(STR(L-4-L2+1:L-4),'(A)') STR2(1:L2)
        
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        WRITE(*,*) TRIM(STR)
        
        WRITE(21,'(A)') 'TITLE="surface_data"'
        WRITE(21,'(A)') 'VARIABLES= "x", "y", "z", "oninterface", "point_type", "force1", "force2", "force3", "r1", "r2", "r3", "relatedpt0", "relatedpt1","relatedfaceC","B_RATE","rel_face_F", "impact_zone_case", "impact_zone_self", "divided_region", "divided_boundary", "point_disp1", "point_disp2", "point_disp3", "mesh_quality"' !, "LENGTH"'
        
        DO J=0,2
            
            IF(J==0) THEN
                SURFACE_CURRENT => SURFACE_FLUID
            ELSE IF(J==1) THEN
                SURFACE_CURRENT => SURFACE_PROPEL
            ELSE IF(J==2) THEN
                SURFACE_CURRENT => SURFACE_CASE
            END IF
            STR3 = ''
            WRITE(STR3, '(F)') SURFACE_TOTAL_TIME
            STR3 = TRIM(ADJUSTL(STR3))
            L3 = LEN_TRIM(STR3)
            
            STR4 = ''
            WRITE(STR4, '(I)') SURFACE_CURRENT%SURFACE_POINTS_NUM
            STR4 = TRIM(ADJUSTL(STR4))
            L4 = LEN_TRIM(STR4)
            
            STR5 = ''
            WRITE(STR5, '(I)') SURFACE_CURRENT%SURFACE_FACES_NUM
            STR5 = TRIM(ADJUSTL(STR5))
            L5 = LEN_TRIM(STR5)
            
            STR6 = ', DATAPACKING = BLOCK, ZONETYPE = FETRIANGLE, VARLOCATION = ([1,2,3,5,6,7,8,9,10,11,12,13,14,16,21,22,23]=NODAL, [4,15,17,18,19,20,24]=CELLCENTERED)'
            STR6 = TRIM(ADJUSTL(STR6))
            L6 = LEN_TRIM(STR6)
            
            IF(J==0) THEN
                WRITE(21,'(A,A,A,A,A,A,A)') 'ZONE SolutionTime = ', STR3(1:L3),', T = "surface_fluid", NODES = ', STR4(1:L4),', ELEMENTS =  ', STR5(1:L5), STR6(1:L6)
            ELSE IF(J==1) THEN
                WRITE(21,'(A,A,A,A,A,A,A)') 'ZONE SolutionTime = ', STR3(1:L3),', T = "surface_propel", NODES = ', STR4(1:L4),', ELEMENTS =  ', STR5(1:L5), STR6(1:L6)
            ELSE IF(J==2) THEN
                WRITE(21,'(A,A,A,A,A,A,A)') 'ZONE SolutionTime = ', STR3(1:L3),', T = "surface_case", NODES = ', STR4(1:L4),', ELEMENTS =  ', STR5(1:L5), STR6(1:L6)
            END IF
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%SURFACE_POINTS(1,I)
            END DO
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%SURFACE_POINTS(2,I)
            END DO
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%SURFACE_POINTS(3,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
                WRITE(21,'(I)') SURFACE_CURRENT%FACE_ONINTERFACE(I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(I)') SURFACE_CURRENT%POINT_TYPE(I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_FORCE(1,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_FORCE(2,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_FORCE(3,I)
            END DO

            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_DISTANCE(1,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_DISTANCE(2,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_DISTANCE(3,I)
            END DO

            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(I)') SURFACE_CURRENT%POINT_RELATEDPT(1,I)
            END DO

            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(I)') SURFACE_CURRENT%POINT_RELATEDPT(2,I)
            END DO

            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(I)') SURFACE_CURRENT%POINT_RELATEDFACE(3,I)
            END DO
            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%FACE_B_RATE(I)
            END DO
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(I)') SURFACE_CURRENT%POINT_RELATEDFACE(1,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
                J0 = SURFACE_CURRENT%FACE_IMPACT_ZONE(2+1, I)
                IF(J0==0) THEN
                    WRITE(21,'(A)') '10000'
                ELSE
		    IF(J==2) THEN
                    CALL DISTANCE_FACE_FACE_TYPE(I,2,J0,2,0,  R,B)
		    ELSE
                    CALL DISTANCE_FACE_FACE_TYPE(I,J,J0,2,1,  R,B)
		    END IF

                    IF(.NOT. B) THEN
                       WRITE(21,'(A)') '10000'
                    ELSE
                        WRITE(21,'(F)') R
                    END IF
                END IF
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
                J0 = SURFACE_CURRENT%FACE_IMPACT_ZONE(J+1, I)
                IF(J0==0) THEN
                    WRITE(21,'(A)') '10000'
                ELSE
                    CALL DISTANCE_FACE_FACE_TYPE(I,J,J0,J,0,  R,B)

                    IF(.NOT. B) THEN
                       WRITE(21,'(A)') '10000'
                    ELSE
                        WRITE(21,'(F)') R
                    END IF
                END IF
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
                WRITE(21,'(I)') SURFACE_CURRENT%FACE_DIVIDED_REGION_ARRAY(I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
                WRITE(21,'(I)') MAX(SURFACE_CURRENT%FACE_DIVIDED_BOUNDARY_ARRAY(1,I), SURFACE_CURRENT%FACE_DIVIDED_BOUNDARY_ARRAY(2,I), SURFACE_CURRENT%FACE_DIVIDED_BOUNDARY_ARRAY(3,I))
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_DISPLACEMENT(1,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_DISPLACEMENT(2,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F)') SURFACE_CURRENT%POINT_DISPLACEMENT(3,I)
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
                CALL MESH_QUALITY_TRIANGLE_ONE(SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(1,I)), SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(2,I)), SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(3,I)), SURFACE_CURRENT%SURFACE_INITIAL_FACE_AREA(I), F_SIZE_SHAPE, F_SIZE, F_SHAPE)
                WRITE(21,'(F)') F_SHAPE
            END DO
            
            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
                WRITE(21,'(I,I,I)') SURFACE_CURRENT%SURFACE_FACES(1,I), SURFACE_CURRENT%SURFACE_FACES(2,I), SURFACE_CURRENT%SURFACE_FACES(3,I)
            END DO
            
        END DO
            
        
    
        CLOSE(21)
    
    END SUBROUTINE SAVINGDATA_TECPLOT

    SUBROUTINE SAVINGINTERFACE_TECPLOT(FILE_NUM)
        IMPLICIT NONE
        INTEGER :: FILE_NUM
        INTEGER :: I
        CHARACTER(500) :: STR, STR2, STR3, STR4, STR5, STR6
        INTEGER :: L, L2, L3, L4, L5, L6
    
        INTEGER :: J
        
        INTEGER :: POINT_NUM
        REAL(8), POINTER, DIMENSION(:,:) :: POINT
        INTEGER, POINTER, DIMENSION(:,:) :: POINT_LOC
        INTEGER :: FACE_NUM
        INTEGER, POINTER, DIMENSION(:,:) :: FACE
        INTEGER, POINTER, DIMENSION(:,:) :: FACE_LOC
        
        CALL UPDATE_INTERFACE_CLUSTER(0)
        CALL UPDATE_INTERFACE_CLUSTER(1)
        
        STR = ''
        WRITE(STR, '(A)') './output/surface/tec_interface_3d/surface_interface_0000000.plt'
        L = LEN_TRIM(STR)
        
        STR2 = ''
        WRITE(STR2, '(I)') FILE_NUM
        STR2 = TRIM(ADJUSTL(STR2))
        L2 = LEN_TRIM(STR2)
        
        WRITE(STR(L-4-L2+1:L-4),'(A)') STR2(1:L2)
        
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        WRITE(*,*) TRIM(STR)
        
        WRITE(21,'(A)') 'TITLE="surface_interface"'
        WRITE(21,'(A)') 'VARIABLES= "x", "y", "z", "force1", "force2", "force3", "pressure", "brate", "face_loc"'
        
        DO J=0,1
        
            IF (J==0) THEN
                POINT_NUM = INTERFACE_FLUID_POINTS_NUM
                POINT => INTERFACE_FLUID_POINTS
                POINT_LOC => INTERFACE_FLUID_POINTS_LOC
                
                FACE_NUM = INTERFACE_FLUID_FACES_NUM
                FACE => INTERFACE_FLUID_FACES
                FACE_LOC => INTERFACE_FLUID_FACES_LOC
            ELSE IF(J==1) THEN
                POINT_NUM = INTERFACE_STRUCT_POINTS_NUM
                POINT => INTERFACE_STRUCT_POINTS
                POINT_LOC => INTERFACE_STRUCT_POINTS_LOC
                
                FACE_NUM = INTERFACE_STRUCT_FACES_NUM
                FACE => INTERFACE_STRUCT_FACES
                FACE_LOC => INTERFACE_STRUCT_FACES_LOC
            END IF
            
            STR3 = ''
            WRITE(STR3, '(F)') SURFACE_TOTAL_TIME
            STR3 = TRIM(ADJUSTL(STR3))
            L3 = LEN_TRIM(STR3)
            
            STR4 = ''
            WRITE(STR4, '(I)') POINT_NUM
            STR4 = TRIM(ADJUSTL(STR4))
            L4 = LEN_TRIM(STR4)
            
            STR5 = ''
            WRITE(STR5, '(I)') FACE_NUM
            STR5 = TRIM(ADJUSTL(STR5))
            L5 = LEN_TRIM(STR5)
            
            STR6 = ', DATAPACKING = BLOCK, ZONETYPE = FETRIANGLE, VARLOCATION = ([1,2,3,4,5,6]=NODAL, [7,8,9]=CELLCENTERED)'
            STR6 = TRIM(ADJUSTL(STR6))
            L6 = LEN_TRIM(STR6)
            
            IF(J==0) THEN
                WRITE(21,'(A,A,A,A,A,A,A)') 'ZONE SolutionTime = ', STR3(1:L3),', T = "surface_fluid", NODES = ', STR4(1:L4),', ELEMENTS =  ', STR5(1:L5), STR6(1:L6)
            ELSE IF(J==1) THEN
                WRITE(21,'(A,A,A,A,A,A,A)') 'ZONE SolutionTime = ', STR3(1:L3),', T = "surface_propel", NODES = ', STR4(1:L4),', ELEMENTS =  ', STR5(1:L5), STR6(1:L6)
            END IF
            
            DO I = 1, POINT_NUM
                WRITE(21,'(F)') POINT(1,I)
            END DO
            DO I = 1, POINT_NUM
                WRITE(21,'(F)') POINT(2,I)
            END DO
            DO I = 1, POINT_NUM
                WRITE(21,'(F)') POINT(3,I)
            END DO
            
            DO I = 1, POINT_NUM
                IF(POINT_LOC(2,I)==0) THEN
                    WRITE(21,'(F)') SURFACE_FLUID%POINT_FORCE(1,POINT_LOC(1,I))
                ELSE IF(POINT_LOC(2,I)==1) THEN
                    WRITE(21,'(F)') SURFACE_PROPEL%POINT_FORCE(1,POINT_LOC(1,I))
                ELSE
                    WRITE(21,'(F)') SURFACE_CASE%POINT_FORCE(1,POINT_LOC(1,I))
                END IF
            END DO
            
            DO I = 1, POINT_NUM
                IF(POINT_LOC(2,I)==0) THEN
                    WRITE(21,'(F)') SURFACE_FLUID%POINT_FORCE(2,POINT_LOC(1,I))
                ELSE IF(POINT_LOC(2,I)==1) THEN
                    WRITE(21,'(F)') SURFACE_PROPEL%POINT_FORCE(2,POINT_LOC(1,I))
                ELSE
                    WRITE(21,'(F)') SURFACE_CASE%POINT_FORCE(2,POINT_LOC(1,I))
                END IF
            END DO
            
            DO I = 1, POINT_NUM
                IF(POINT_LOC(2,I)==0) THEN
                    WRITE(21,'(F)') SURFACE_FLUID%POINT_FORCE(3,POINT_LOC(1,I))
                ELSE IF(POINT_LOC(2,I)==1) THEN
                    WRITE(21,'(F)') SURFACE_PROPEL%POINT_FORCE(3,POINT_LOC(1,I))
                ELSE
                    WRITE(21,'(F)') SURFACE_CASE%POINT_FORCE(3,POINT_LOC(1,I))
                END IF
            END DO
            
            DO I = 1, FACE_NUM
                IF(FACE_LOC(2,I)==0) THEN
		    WRITE(21,'(F)') SURFACE_FLUID%FACE_PRESSURE(FACE_LOC(1,I))
                ELSE IF(FACE_LOC(2,I)==1) THEN
                    WRITE(21,'(F)') SURFACE_PROPEL%FACE_PRESSURE(FACE_LOC(1,I))
                ELSE IF(FACE_LOC(2,I)==2) THEN
                    WRITE(21,'(F)') SURFACE_CASE%FACE_PRESSURE(FACE_LOC(1,I))
		ELSE IF(FACE_LOC(2,I)==-1) THEN
                    WRITE(21,'(F)') 0.0
                END IF
            END DO
            
            DO I = 1, FACE_NUM
                IF(FACE_LOC(2,I)==0) THEN
		    WRITE(21,'(F)') SURFACE_FLUID%FACE_B_RATE(FACE_LOC(1,I))
                ELSE IF(FACE_LOC(2,I)==1) THEN
                    WRITE(21,'(F)') SURFACE_PROPEL%FACE_B_RATE(FACE_LOC(1,I))
		ELSE IF(FACE_LOC(2,I)==2) THEN
                    WRITE(21,'(F)') SURFACE_CASE%FACE_B_RATE(FACE_LOC(1,I))
		ELSE IF(FACE_LOC(2,I)==-1) THEN
                    WRITE(21,'(F)') 0.0
                END IF
            END DO

            DO I = 1, FACE_NUM
                    WRITE(21,'(I)') FACE_LOC(2,I)
            END DO
         
            DO I = 1, FACE_NUM
                WRITE(21,'(I,I,I)') FACE(1,I), FACE(2,I), FACE(3,I)
            END DO
            
        END DO
    
        CLOSE(21)
    
    END SUBROUTINE SAVINGINTERFACE_TECPLOT
    
    
    SUBROUTINE SAVINGBRATE(FILE_NUM, TYP)
        IMPLICIT NONE
        INTEGER :: TYP
        
        INTEGER :: FILE_NUM
        INTEGER :: I
        CHARACTER(500) :: STR
        
        INTEGER :: FACE_NUM
        REAL(8), POINTER, DIMENSION(:) :: B_RATE
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        
        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        ELSE IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        ELSE IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF
        
        FACE_NUM = SURFACE_CURRENT%SURFACE_FACES_NUM
        B_RATE => SURFACE_CURRENT%FACE_B_RATE
        
        WRITE(STR, *), FILE_NUM
        STR = './output/surface/brates/brate_3d' // TRIM(ADJUSTL(STR)) // '.txt'
        WRITE(*,*) TRIM(STR)
    
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        
        !$OMP DO ORDERED 
        DO I = 1, FACE_NUM
            WRITE(21,*) B_RATE(I)
        END DO
    	!$OMP END DO 
    
        CLOSE(21)
    END SUBROUTINE SAVINGBRATE
    
    SUBROUTINE SAVINGDISPLACEMENT(FILE_NUM, TYP)
	IMPLICIT NONE
        INTEGER :: TYP
        
        INTEGER :: FILE_NUM
        INTEGER :: I
        CHARACTER(500) :: STR, STR2
        
        INTEGER :: POINT_NUM
        REAL(8), POINTER, DIMENSION(:,:) :: DISPLACEMENT
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        
        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        ELSE IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        ELSE IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF
        
            POINT_NUM = SURFACE_CURRENT%SURFACE_POINTS_NUM
            DISPLACEMENT => SURFACE_CURRENT%POINT_VELOCITY

        WRITE(STR, *), FILE_NUM
        WRITE(STR2, *), TYP
        STR = './output/surface/displacements/displacement_3d' // TRIM(ADJUSTL(STR)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        WRITE(*,*) TRIM(STR)
    
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        
        !$OMP DO ORDERED 
        DO I = 1, POINT_NUM
            WRITE(21,*) DISPLACEMENT(1,I)
            WRITE(21,*) DISPLACEMENT(2,I)
            WRITE(21,*) DISPLACEMENT(3,I)
        END DO
    	!$OMP END DO
    
        CLOSE(21)
    END SUBROUTINE SAVINGDISPLACEMENT
    
    SUBROUTINE SAVINGPRESSURE(FILE_NUM, TYP)
	IMPLICIT NONE
        INTEGER :: TYP
        
        INTEGER :: FILE_NUM
        INTEGER :: I
        CHARACTER(500) :: STR, STR2
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        
        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        ELSE IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        ELSE IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF

        WRITE(STR, *), FILE_NUM
        WRITE(STR2, *), TYP
        STR = './output/surface/pressures/pressure_3d' // TRIM(ADJUSTL(STR)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        WRITE(*,*) TRIM(STR)
    
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        
        IF(TYP==0) THEN
            !$OMP DO ORDERED 
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,*) SURFACE_CURRENT%FACE_PRESSURE(I)
            END DO
    	    !$OMP END DO
        ELSE
            !$OMP DO ORDERED 
            DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
                WRITE(21,'(F,F,F)') SURFACE_CURRENT%POINT_FORCE(1,I), SURFACE_CURRENT%POINT_FORCE(2,I), SURFACE_CURRENT%POINT_FORCE(3,I)
            END DO
    	    !$OMP END DO
        END IF
    
        CLOSE(21)
    END SUBROUTINE SAVINGPRESSURE

       SUBROUTINE SAVING_SURFACE(FILE_NUM, TYP)
	IMPLICIT NONE

        INTEGER :: TYP
        
        INTEGER :: FILE_NUM
        INTEGER :: I, J
        CHARACTER(500) :: STR, STR1, STR2
        
	TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        
         
        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        END IF
        IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        END IF
        IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF
        
        WRITE(STR1, *), FILE_NUM
        WRITE(STR2, *), TYP
    
        STR = './output/surface/restart_3d/global_variables_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")

        WRITE(21,*) SURFACE_CURRENT%MESH_SIZE
        WRITE(21,*) SURFACE_CURRENT%MESH_SIZE_MAX
        WRITE(21,*) DOMAIN_MAX
        WRITE(21,*) DOMAIN_MIN
        WRITE(21,*) SURFACE_AREA_ITER
        WRITE(21,*) SURFACE_PRESSURE_ITER
        WRITE(21,*) SURFACE_AREA_ARRAY
        WRITE(21,*) TOTAL_PRESSURE_ARRAY
        WRITE(21,*) SURFACE_FLAG_ARRAY
	WRITE(21,*) MODEL_ANGLE
	WRITE(21,*) MODEL_AXIS_EQN(1,1), MODEL_AXIS_EQN(2,1), MODEL_AXIS_EQN(3,1), MODEL_AXIS_EQN(4,1)
	WRITE(21,*) MODEL_AXIS_EQN(1,2), MODEL_AXIS_EQN(2,2), MODEL_AXIS_EQN(3,2), MODEL_AXIS_EQN(4,2)
        WRITE(21,*) CHI_C_LOW
        WRITE(21,*) CHI_C_HIGH
        WRITE(21,*) INTERFACE_THRESHOLD
        WRITE(21,*) SMALL_REGION_POINT_NUM
        WRITE(21,*) THIN_REGION_EXISTENCE
        WRITE(21,*) THIN_REGION_ATTACHMENT
        WRITE(21,*) EDGE_SPLITTING
        WRITE(21,*) EDGE_COLLAPSING

        CLOSE(21)

        STR = './output/surface/restart_3d/mesh_num_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")

        WRITE(21,*) SURFACE_CURRENT%SURFACE_POINTS_NUM
        WRITE(21,*) SURFACE_CURRENT%SURFACE_FACES_NUM
	WRITE(21,*) SURFACE_CURRENT%SURFACE_PATCHES_NUM
	WRITE(21,*) INTERFACE_FLUID_POINTS_NUM
	WRITE(21,*) INTERFACE_STRUCT_POINTS_NUM
        CLOSE(21)


        STR = './output/surface/restart_3d/point_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
            WRITE(21,*) SURFACE_CURRENT%SURFACE_POINTS(1,I)
            WRITE(21,*) SURFACE_CURRENT%SURFACE_POINTS(2,I)
            WRITE(21,*) SURFACE_CURRENT%SURFACE_POINTS(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/point_velocity_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
            WRITE(21,*) SURFACE_CURRENT%POINT_VELOCITY(1,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_VELOCITY(2,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_VELOCITY(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/point_displacement_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
            WRITE(21,*) SURFACE_CURRENT%POINT_DISPLACEMENT(1,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_DISPLACEMENT(2,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_DISPLACEMENT(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/point_force_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
            WRITE(21,*) SURFACE_CURRENT%POINT_FORCE(1,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_FORCE(2,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_FORCE(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)


        STR = './output/surface/restart_3d/point_face_connection_num_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
            WRITE(21,*) SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
        END DO
        !$OMP END DO
        CLOSE(21)


        STR = './output/surface/restart_3d/point_face_connection_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
	    DO J = 1, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
		WRITE(21,*) SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)
	    END DO
        END DO
        !$OMP END DO
        CLOSE(21)


        STR = './output/surface/restart_3d/point_type_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
            WRITE(21,*) SURFACE_CURRENT%POINT_TYPE(I)
        END DO
        !$OMP END DO
        CLOSE(21)


        STR = './output/surface/restart_3d/point_relatedpt_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
            WRITE(21,*) SURFACE_CURRENT%POINT_RELATEDPT(1,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_RELATEDPT(2,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_RELATEDPT(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)


        STR = './output/surface/restart_3d/point_relatedface_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
            WRITE(21,*) SURFACE_CURRENT%POINT_RELATEDFACE(1,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_RELATEDFACE(2,I)
            WRITE(21,*) SURFACE_CURRENT%POINT_RELATEDFACE(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/face_b_rate_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%FACE_B_RATE(I)
        END DO
        !$OMP END DO
        CLOSE(21)

         STR = './output/surface/restart_3d/initial_face_area_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%SURFACE_INITIAL_FACE_AREA(I)
        END DO
        !$OMP END DO
        CLOSE(21)

         STR = './output/surface/restart_3d/initial_edge_length_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%SURFACE_INITIAL_EDGE_LENGTH(1,I)
            WRITE(21,*) SURFACE_CURRENT%SURFACE_INITIAL_EDGE_LENGTH(2,I)
            WRITE(21,*) SURFACE_CURRENT%SURFACE_INITIAL_EDGE_LENGTH(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)


        STR = './output/surface/restart_3d/face_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%SURFACE_FACES(1,I)
            WRITE(21,*) SURFACE_CURRENT%SURFACE_FACES(2,I)
            WRITE(21,*) SURFACE_CURRENT%SURFACE_FACES(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/face_location_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%FACE_LOCATION(I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/face_oninterface_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%FACE_ONINTERFACE(I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/face_pressure_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%FACE_PRESSURE(I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/face_impact_zone_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%FACE_IMPACT_ZONE(1,I)
            WRITE(21,*) SURFACE_CURRENT%FACE_IMPACT_ZONE(2,I)
            WRITE(21,*) SURFACE_CURRENT%FACE_IMPACT_ZONE(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/face_ablation_flag_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            WRITE(21,*) SURFACE_CURRENT%FACE_ABLATION_FLAG(I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/topchange_typ_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_PATCHES_NUM
            WRITE(21,*) SURFACE_CURRENT%SURFACE_PATCHES_TOPCHANGE_TYP(I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/interface_fluid_points_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, INTERFACE_FLUID_POINTS_NUM
            WRITE(21,*) INTERFACE_FLUID_POINTS(1,I)
    	    WRITE(21,*) INTERFACE_FLUID_POINTS(2,I)
    	    WRITE(21,*) INTERFACE_FLUID_POINTS(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/interface_fluid_points_loc_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, INTERFACE_FLUID_POINTS_NUM
            WRITE(21,*) INTERFACE_FLUID_POINTS_LOC(1,I)
    	    WRITE(21,*) INTERFACE_FLUID_POINTS_LOC(2,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/interface_struct_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, INTERFACE_STRUCT_POINTS_NUM
            WRITE(21,*) INTERFACE_STRUCT_POINTS(1,I)
    	    WRITE(21,*) INTERFACE_STRUCT_POINTS(2,I)
    	    WRITE(21,*) INTERFACE_STRUCT_POINTS(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)

        STR = './output/surface/restart_3d/interface_struct_points_loc_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        !$OMP DO ORDERED 
        DO I = 1, INTERFACE_STRUCT_POINTS_NUM
            WRITE(21,*) INTERFACE_STRUCT_POINTS_LOC(1,I)
    	    WRITE(21,*) INTERFACE_STRUCT_POINTS_LOC(2,I)
        END DO
        !$OMP END DO
        CLOSE(21)

    END SUBROUTINE SAVING_SURFACE

    SUBROUTINE READ_RESTART(TYP)

    INTEGER :: TYP, IO, I, J
    CHARACTER(500) :: STR, STR1, STR2
     TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT


	WRITE(STR1,*) RESTART_ITERATION
        WRITE(STR2,*) TYP
	
	IF(TYP==0) THEN
	SURFACE_CURRENT => SURFACE_FLUID
	ELSEIF(TYP==1) THEN
	SURFACE_CURRENT => SURFACE_PROPEL
	ELSE
	SURFACE_CURRENT => SURFACE_CASE
	END IF	 

	WRITE(*,*) 'START_READING_SURFACE_INFORMATION, surface_type' // TRIM(ADJUSTL(STR2))  

        STR = './output/surface/restart_3d/global_variables_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(UNIT=21, FILE = STR)

        READ(21,*) SURFACE_CURRENT%MESH_SIZE
        READ(21,*) SURFACE_CURRENT%MESH_SIZE_MAX
        READ(21,*) DOMAIN_MAX
        READ(21,*) DOMAIN_MIN
        READ(21,*) SURFACE_AREA_ITER
        READ(21,*) SURFACE_PRESSURE_ITER
        READ(21,*) SURFACE_AREA_ARRAY
        READ(21,*) TOTAL_PRESSURE_ARRAY
        READ(21,*) SURFACE_FLAG_ARRAY
	READ(21,*) MODEL_ANGLE
	READ(21,*) MODEL_AXIS_EQN(1,1), MODEL_AXIS_EQN(2,1), MODEL_AXIS_EQN(3,1), MODEL_AXIS_EQN(4,1)
	READ(21,*) MODEL_AXIS_EQN(1,2), MODEL_AXIS_EQN(2,2), MODEL_AXIS_EQN(3,2), MODEL_AXIS_EQN(4,2)
        READ(21,*) CHI_C_LOW
        READ(21,*) CHI_C_HIGH
        READ(21,*) INTERFACE_THRESHOLD
        READ(21,*) SMALL_REGION_POINT_NUM
        READ(21,*) THIN_REGION_EXISTENCE
        READ(21,*) THIN_REGION_ATTACHMENT
        READ(21,*) EDGE_SPLITTING
        READ(21,*) EDGE_COLLAPSING

        CLOSE(21)


        STR = './output/surface/restart_3d/mesh_num_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
        OPEN(NEWUNIT=IO,FILE= STR)
    	    READ(IO,*) SURFACE_CURRENT%SURFACE_POINTS_NUM
    	    READ(IO,*) SURFACE_CURRENT%SURFACE_FACES_NUM
    	    READ(IO,*) SURFACE_CURRENT%SURFACE_PATCHES_NUM
    	    READ(IO,*) INTERFACE_FLUID_POINTS_NUM
    	    READ(IO,*) INTERFACE_STRUCT_POINTS_NUM		
        CLOSE(IO)

        STR = './output/surface/restart_3d/point_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%SURFACE_POINTS(3,SURFACE_CURRENT%SURFACE_POINTS_NUM))
        OPEN(NEWUNIT=IO,FILE= STR)
	    DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
		READ(IO,*) SURFACE_CURRENT%SURFACE_POINTS(1,I)
	        READ(IO,*) SURFACE_CURRENT%SURFACE_POINTS(2,I)
                READ(IO,*) SURFACE_CURRENT%SURFACE_POINTS(3,I)
	    END DO		
        CLOSE(IO)

        STR = './output/surface/restart_3d/point_velocity_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%POINT_VELOCITY(3,SURFACE_CURRENT%SURFACE_POINTS_NUM))
        OPEN(NEWUNIT=IO,FILE= STR)
	    DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
		READ(IO,*) SURFACE_CURRENT%POINT_VELOCITY(1,I)
	        READ(IO,*) SURFACE_CURRENT%POINT_VELOCITY(2,I)
                READ(IO,*) SURFACE_CURRENT%POINT_VELOCITY(3,I)
	    END DO		
        CLOSE(IO)

        STR = './output/surface/restart_3d/point_displacement_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%POINT_DISPLACEMENT(3,SURFACE_CURRENT%SURFACE_POINTS_NUM))
        OPEN(NEWUNIT=IO,FILE= STR)
	    DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
		READ(IO,*) SURFACE_CURRENT%POINT_DISPLACEMENT(1,I)
	        READ(IO,*) SURFACE_CURRENT%POINT_DISPLACEMENT(2,I)
	        READ(IO,*) SURFACE_CURRENT%POINT_DISPLACEMENT(3,I)
	    END DO		
        CLOSE(IO)

        STR = './output/surface/restart_3d/point_force_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%POINT_FORCE(3,SURFACE_CURRENT%SURFACE_POINTS_NUM))
        OPEN(NEWUNIT=IO,FILE= STR)
	    DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
		READ(IO,*) SURFACE_CURRENT%POINT_FORCE(1,I)
	        READ(IO,*) SURFACE_CURRENT%POINT_FORCE(2,I)
	        READ(IO,*) SURFACE_CURRENT%POINT_FORCE(3,I)
	    END DO		
        CLOSE(IO)

        STR = './output/surface/restart_3d/point_face_connection_num_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT%SURFACE_POINTS_NUM))
        OPEN(NEWUNIT=IO,FILE= STR)
	    DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
		READ(IO,*) SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
	    END DO		
        CLOSE(IO)

        STR = './output/surface/restart_3d/point_face_connection_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%POINT_FACE_CONNECTION(30,SURFACE_CURRENT%SURFACE_POINTS_NUM))
        OPEN(NEWUNIT=IO,FILE= STR)
	    DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
		DO J = 1, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
		    READ(IO,*) SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)
	        END DO
	    END DO		
        CLOSE(IO)

	STR = './output/surface/restart_3d/point_type_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%POINT_TYPE(SURFACE_CURRENT%SURFACE_POINTS_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
	    READ(21,*) SURFACE_CURRENT%POINT_TYPE(I)
	END DO
	!$OMP END DO
	CLOSE(21)


	STR = './output/surface/restart_3d/point_relatedpt_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%POINT_RELATEDPT(3,SURFACE_CURRENT%SURFACE_POINTS_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
	    READ(21,*) SURFACE_CURRENT%POINT_RELATEDPT(1,I)
	    READ(21,*) SURFACE_CURRENT%POINT_RELATEDPT(2,I)
	    READ(21,*) SURFACE_CURRENT%POINT_RELATEDPT(3,I)
	END DO
	!$OMP END DO
	CLOSE(21)


	STR = './output/surface/restart_3d/point_relatedface_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%POINT_RELATEDFACE(3,SURFACE_CURRENT%SURFACE_POINTS_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_POINTS_NUM
	    READ(21,*) SURFACE_CURRENT%POINT_RELATEDFACE(1,I)
	    READ(21,*) SURFACE_CURRENT%POINT_RELATEDFACE(2,I)
	    READ(21,*) SURFACE_CURRENT%POINT_RELATEDFACE(3,I)
	END DO
	!$OMP END DO
	CLOSE(21)

	STR = './output/surface/restart_3d/face_b_rate_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%FACE_B_RATE(SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%FACE_B_RATE(I)
	END DO
	!$OMP END DO
	CLOSE(21)

	 STR = './output/surface/restart_3d/initial_face_area_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%SURFACE_INITIAL_FACE_AREA(SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%SURFACE_INITIAL_FACE_AREA(I)
	END DO
	!$OMP END DO
	CLOSE(21)

         STR = './output/surface/restart_3d/initial_edge_length_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%SURFACE_INITIAL_EDGE_LENGTH(3,SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
        !$OMP DO ORDERED 
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            READ(21,*) SURFACE_CURRENT%SURFACE_INITIAL_EDGE_LENGTH(1,I)
            READ(21,*) SURFACE_CURRENT%SURFACE_INITIAL_EDGE_LENGTH(2,I)
            READ(21,*) SURFACE_CURRENT%SURFACE_INITIAL_EDGE_LENGTH(3,I)
        END DO
        !$OMP END DO
        CLOSE(21)


	STR = './output/surface/restart_3d/face_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%SURFACE_FACES(3,SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%SURFACE_FACES(1,I)
	    READ(21,*) SURFACE_CURRENT%SURFACE_FACES(2,I)
	    READ(21,*) SURFACE_CURRENT%SURFACE_FACES(3,I)
	END DO
	!$OMP END DO
	CLOSE(21)

	STR = './output/surface/restart_3d/face_location_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%FACE_LOCATION(SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%FACE_LOCATION(I)
	END DO
	!$OMP END DO
	CLOSE(21)

	STR = './output/surface/restart_3d/face_oninterface_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%FACE_ONINTERFACE(SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%FACE_ONINTERFACE(I)
	END DO
	!$OMP END DO
	CLOSE(21)

	STR = './output/surface/restart_3d/face_pressure_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%FACE_PRESSURE(SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%FACE_PRESSURE(I)
	END DO
	!$OMP END DO
	CLOSE(21)

	STR = './output/surface/restart_3d/face_impact_zone_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%FACE_IMPACT_ZONE(3,SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%FACE_IMPACT_ZONE(1,I)
	    READ(21,*) SURFACE_CURRENT%FACE_IMPACT_ZONE(2,I)
	    READ(21,*) SURFACE_CURRENT%FACE_IMPACT_ZONE(3,I)
	END DO
	!$OMP END DO
	CLOSE(21)

	STR = './output/surface/restart_3d/face_ablation_flag_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%FACE_ABLATION_FLAG(SURFACE_CURRENT%SURFACE_FACES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%FACE_ABLATION_FLAG(I)
	END DO
	!$OMP END DO
	CLOSE(21)

	STR = './output/surface/restart_3d/topchange_typ_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
	ALLOCATE(SURFACE_CURRENT%SURFACE_PATCHES_TOPCHANGE_TYP(SURFACE_CURRENT%SURFACE_PATCHES_NUM))
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_PATCHES_NUM
	    READ(21,*) SURFACE_CURRENT%SURFACE_PATCHES_TOPCHANGE_TYP(I)
	END DO
	!$OMP END DO
	CLOSE(21)

	IF(TYP .EQ. 0) THEN
		STR = './output/surface/restart_3d/interface_fluid_points_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
		ALLOCATE(INTERFACE_FLUID_POINTS(3,INTERFACE_FLUID_POINTS_NUM))
		OPEN(UNIT=21, FILE = STR)
		!$OMP DO ORDERED 
		DO I = 1, INTERFACE_FLUID_POINTS_NUM
		    READ(21,*) INTERFACE_FLUID_POINTS(1,I)
		    READ(21,*) INTERFACE_FLUID_POINTS(2,I)
		    READ(21,*) INTERFACE_FLUID_POINTS(3,I)
		END DO
		!$OMP END DO
		CLOSE(21)

		STR = './output/surface/restart_3d/interface_fluid_points_loc_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
		ALLOCATE(INTERFACE_FLUID_POINTS_LOC(2,INTERFACE_FLUID_POINTS_NUM))
		OPEN(UNIT=21, FILE = STR)
		!$OMP DO ORDERED 
		DO I = 1, INTERFACE_FLUID_POINTS_NUM
		    READ(21,*) INTERFACE_FLUID_POINTS_LOC(1,I)
		    READ(21,*) INTERFACE_FLUID_POINTS_LOC(2,I)
		END DO
		!$OMP END DO
		CLOSE(21)


		STR = './output/surface/restart_3d/interface_struct_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
		ALLOCATE(INTERFACE_STRUCT_POINTS(3,INTERFACE_STRUCT_POINTS_NUM))
		OPEN(UNIT=21, FILE = STR)
		!$OMP DO ORDERED 
		DO I = 1, INTERFACE_STRUCT_POINTS_NUM
		    READ(21,*) INTERFACE_STRUCT_POINTS(1,I)
		    READ(21,*) INTERFACE_STRUCT_POINTS(2,I)
		    READ(21,*) INTERFACE_STRUCT_POINTS(3,I)
		END DO
		!$OMP END DO
		CLOSE(21)

		STR = './output/surface/restart_3d/interface_struct_points_loc_3d' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'
		ALLOCATE(INTERFACE_STRUCT_POINTS_LOC(2,INTERFACE_STRUCT_POINTS_NUM))
		OPEN(UNIT=21, FILE = STR)
		!$OMP DO ORDERED 
		DO I = 1, INTERFACE_STRUCT_POINTS_NUM
		    READ(21,*) INTERFACE_STRUCT_POINTS_LOC(1,I)
		    READ(21,*) INTERFACE_STRUCT_POINTS_LOC(2,I)
		END DO
		!$OMP END DO
		CLOSE(21)
	END IF 

	WRITE(*,*) 'END_READING_SURFACE_INFORMATION, surface_type' // TRIM(ADJUSTL(STR2))  

END SUBROUTINE READ_RESTART
    
     SUBROUTINE COMPUTE_SURFACE_AREA(TYP,ANGLE)
	IMPLICIT NONE
        INTEGER :: TYP
        
        INTEGER :: I, I1, I2, I3
        REAL(8) :: S, R(3), V1(3), V2(3)
	REAL(8) :: ANGLE, TEMP
        CHARACTER(500) :: STR, STR2
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        
        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        ELSE IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        ELSE IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF
        
        S = 0.
        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
            IF(SURFACE_CURRENT%FACE_ONINTERFACE(I)==1-TYP) THEN
                I1 = SURFACE_CURRENT%SURFACE_FACES(1,I)
                I2 = SURFACE_CURRENT%SURFACE_FACES(2,I)
                I3 = SURFACE_CURRENT%SURFACE_FACES(3,I)
                V1 = SURFACE_CURRENT%SURFACE_POINTS(:,I3) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
		V2 = SURFACE_CURRENT%SURFACE_POINTS(:,I2) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
                          
                CALL VEC_CURL1(V1,V2,R)
		TEMP = SQRT(DOT_PRODUCT(R,R))
		S = S + TEMP/2.
            END IF
        END DO
        SURFACE_AREA_ARRAY(SURFACE_AREA_ITER) = S*360./REAL(ANGLE)
        
        WRITE(STR2, *), TYP
        STR = './output/surface/surfacearea3d_' // TRIM(ADJUSTL(STR2)) // '.txt'
        WRITE(*,*) TRIM(STR)
        
        OPEN(UNIT=21, FILE = STR, ACTION = "WRITE", STATUS = "REPLACE")
        
        DO I = 1, SURFACE_AREA_ITER
            WRITE(21,'(F)') SURFACE_AREA_ARRAY(I)
        END DO
    
        CLOSE(21)
        
    END SUBROUTINE COMPUTE_SURFACE_AREA
END MODULE



!! THIS SUBROUTINE IS MAIN SUBROUTINE OF SURFACE_3D MODULE
    

SUBROUTINE SURFACE_3D(TYPE1, TYPE2, TIMESTEP, &
           F_POINT_NUM,F_POINT,F_FACE_NUM,F_FACE,F_LOC,F_B_RATE,F_PRESSURE, F_BCFLAG, &
           P_POINT_NUM,P_POINT,P_FACE_NUM,P_FACE,P_LOC,P_VELOCITY,P_DISPLACEMENT,P_FORCE,P_FACE_AREA,P_CORNER_INDEX, &
           C_POINT_NUM,C_POINT,C_FACE_NUM,C_FACE,C_VELOCITY,C_DISPLACEMENT,C_FORCE, &
           FLAG, FLAG_ARRAY, FILENUM, PATCHNUM)

    USE SURFACE_MODULE_3D
    USE PROPA_RECONST_REINITIAL_3D
    USE SURFACE_EXTERNAL_COMMON_3D
    USE REMESHING_3D
    IMPLICIT NONE
    INTEGER, OPTIONAL :: TYPE1
    INTEGER, OPTIONAL :: TYPE2

    REAL(8), OPTIONAL :: TIMESTEP
        
    INTEGER, OPTIONAL :: F_POINT_NUM
    REAL(8), ALLOCATABLE, OPTIONAL :: F_POINT(:,:)
    INTEGER, OPTIONAL :: F_FACE_NUM
    INTEGER, ALLOCATABLE, OPTIONAL :: F_FACE(:,:)
    INTEGER, ALLOCATABLE, OPTIONAL :: F_LOC(:)
    REAL(8), OPTIONAL :: F_B_RATE(:)
    REAL(8), OPTIONAL :: F_PRESSURE(:)
    INTEGER, ALLOCATABLE, OPTIONAL :: F_BCFLAG(:)
        
    INTEGER, OPTIONAL :: P_POINT_NUM
    REAL(8), ALLOCATABLE, OPTIONAL :: P_POINT(:,:)
    INTEGER, OPTIONAL :: P_FACE_NUM
    INTEGER, ALLOCATABLE, OPTIONAL :: P_FACE(:,:)
    INTEGER, ALLOCATABLE, OPTIONAL :: P_LOC(:)
    REAL(8), OPTIONAL :: P_VELOCITY(:,:)
    REAL(8), OPTIONAL :: P_DISPLACEMENT(:,:)
    REAL(8), OPTIONAL :: P_FORCE(:,:)
    
    REAL(8), ALLOCATABLE, OPTIONAL :: P_FACE_AREA(:)
    INTEGER, ALLOCATABLE, OPTIONAL :: P_CORNER_INDEX(:)
        
    INTEGER, OPTIONAL :: C_POINT_NUM
    REAL(8), ALLOCATABLE, OPTIONAL :: C_POINT(:,:)
    INTEGER, OPTIONAL :: C_FACE_NUM
    INTEGER, ALLOCATABLE, OPTIONAL :: C_FACE(:,:)
    REAL(8), OPTIONAL :: C_VELOCITY(:,:)
    REAL(8), OPTIONAL :: C_DISPLACEMENT(:,:)
    REAL(8), OPTIONAL :: C_FORCE(:,:)
        
    LOGICAL, OPTIONAL :: FLAG
    INTEGER, ALLOCATABLE, OPTIONAL :: FLAG_ARRAY(:)
        
    INTEGER, OPTIONAL :: FILENUM
    INTEGER, OPTIONAL :: PATCHNUM
        
    REAL(8) :: MINX, MAXX, MINY, MAXY, MINZ, MAXZ, TEMP
    INTEGER, ALLOCATABLE :: TEMP_ONINTERFACE(:), TEMP_ONINTERFACE2(:)
    INTEGER, ALLOCATABLE :: TEMP_CONNECTION_NUM(:), TEMP_CONNECTION_NUM2(:)
    INTEGER, ALLOCATABLE :: TEMP_CONNECTION(:,:), TEMP_CONNECTION2(:,:)
    
    LOGICAL :: TEMPFLAG
    
    INTEGER :: I
    INTEGER, ALLOCATABLE :: TEMP_FACE(:,:), TEMP_FACE2(:,:)
    INTEGER, ALLOCATABLE :: TEMP_LOC(:), TEMP_LOC2(:)

    INTEGER :: TYP1_POINT_NUM
    REAL(8), ALLOCATABLE :: TYP1_POINT(:,:)
    INTEGER, ALLOCATABLE :: TYP1_POINTLOC(:,:)
    INTEGER :: TYP1_FACE_NUM
    INTEGER, ALLOCATABLE :: TYP1_FACE(:,:)
    REAL(8), ALLOCATABLE :: DATA_DISPLACEMENT(:,:)
    
    IF(TYPE1==-2) THEN
        
        WRITE(*,*) 'SURFACE FINILAZATION STARTED'
        
        CALL RESET_SURFACE(0, .FALSE., 0, 0)
        CALL RESET_SURFACE(1, .FALSE., 0, 0)
        CALL RESET_SURFACE(2, .FALSE., 0, 0)
        
        IF(ALLOCATED(INTERFACE_FLUID_POINTS)) THEN
            DEALLOCATE(INTERFACE_FLUID_POINTS)
        END IF
        IF(ALLOCATED(INTERFACE_FLUID_POINTS_LOC)) THEN
            DEALLOCATE(INTERFACE_FLUID_POINTS_LOC)
        END IF
        IF(ALLOCATED(INTERFACE_FLUID_FACES)) THEN
            DEALLOCATE(INTERFACE_FLUID_FACES)
        END IF
        IF(ALLOCATED(INTERFACE_FLUID_FACES_LOC)) THEN
            DEALLOCATE(INTERFACE_FLUID_FACES_LOC)
        END IF
        
        IF(ALLOCATED(INTERFACE_STRUCT_POINTS)) THEN
            DEALLOCATE(INTERFACE_STRUCT_POINTS)
        END IF
        IF(ALLOCATED(INTERFACE_STRUCT_POINTS_LOC)) THEN
            DEALLOCATE(INTERFACE_STRUCT_POINTS_LOC)
        END IF
        IF(ALLOCATED(INTERFACE_STRUCT_FACES)) THEN
            DEALLOCATE(INTERFACE_STRUCT_FACES)
        END IF
        IF(ALLOCATED(INTERFACE_STRUCT_FACES_LOC)) THEN
            DEALLOCATE(INTERFACE_STRUCT_FACES_LOC)
        END IF
        
        WRITE(*,*) 'SURFACE FINILAZATION FINISHED'
    
    END IF
        
    IF(TYPE1==-1) THEN

	CALL READINPUT

	SELECT CASE(RESTART_FLAG)
	CASE(0)
	  WRITE(*,*) 'SURFACE >> Start from Restart File'
	CASE(1)
	  WRITE(*,*) 'SURFACE >> Start from Initial Condition'
	CASE DEFAULT
	  WRITE(*,*) 'SURFACE >> Invalid Restart Flag'
	  STOP
	END SELECT
        
	IF(RESTART_FLAG .EQ. 1) THEN
        
        WRITE(*,*) 'SURFACE INITIALIZATION STARTED'
        CALL INIT_RANDOM_SEED()
        
        !MODEL_ANGLE = 30.0
        
        SURFACE_TOTAL_TIME = 0.
	SURFACE_AREA_ITER = 0
	SURFACE_PRESSURE_ITER = 0
        
        SURFACE_FLUID%SURFACE_POINTS_NUM = 0
        SURFACE_FLUID%SURFACE_FACES_NUM = 0
        SURFACE_PROPEL%SURFACE_POINTS_NUM = 0
        SURFACE_PROPEL%SURFACE_FACES_NUM = 0
        SURFACE_CASE%SURFACE_POINTS_NUM = 0
        SURFACE_CASE%SURFACE_FACES_NUM = 0
        
        INTERFACE_FLUID_POINTS_NUM = 0
        INTERFACE_FLUID_FACES_NUM = 0
        INTERFACE_STRUCT_POINTS_NUM = 0
        INTERFACE_STRUCT_FACES_NUM = 0
       
        MINX = F_POINT(1,1)
        MAXX = F_POINT(1,1)
        MINY = F_POINT(2,1)
        MAXY = F_POINT(2,1)
        MINZ = F_POINT(3,1)
        MAXZ = F_POINT(3,1)
	!$OMP PARALLEL DO PRIVATE(I),REDUCTION(MAX:MAXX,MAXY), REDUCTION(MIN:MINX,MINY)
        DO I = 2, F_POINT_NUM
            IF(F_POINT(1,I) < MINX) THEN
                MINX = F_POINT(1,I)
            ENDIF
            IF(F_POINT(1,I) > MAXX) THEN
                MAXX = F_POINT(1,I)
            ENDIF
            IF(F_POINT(2,I) < MINY) THEN
                MINY = F_POINT(2,I)
            ENDIF
            IF(F_POINT(2,I) > MAXY) THEN
                MAXY = F_POINT(2,I)
            ENDIF
            IF(F_POINT(3,I) < MINZ) THEN
                MINZ = F_POINT(3,I)
            ENDIF
            IF(F_POINT(3,I) > MAXZ) THEN
                MAXZ = F_POINT(3,I)
            ENDIF
        END DO
        !$OMP END PARALLEL DO
        
        !$OMP PARALLEL DO PRIVATE(I), REDUCTION(MAX:MAXX,MAXY), REDUCTION(MIN:MINX,MINY)
        DO I = 2, P_POINT_NUM
            IF(P_POINT(1,I) < MINX) THEN
                MINX = P_POINT(1,I)
            ENDIF
            IF(P_POINT(1,I) > MAXX) THEN
                MAXX = P_POINT(1,I)
            ENDIF
            IF(P_POINT(2,I) < MINY) THEN
                MINY = P_POINT(2,I)
            ENDIF
            IF(P_POINT(2,I) > MAXY) THEN
                MAXY = P_POINT(2,I)
            ENDIF
            IF(P_POINT(3,I) < MINZ) THEN
                MINZ = P_POINT(3,I)
            ENDIF
            IF(P_POINT(3,I) > MAXY) THEN
                MAXZ = P_POINT(3,I)
            ENDIF
        END DO
        !$OMP END PARALLEL DO
        
        !$OMP PARALLEL DO PRIVATE(I),REDUCTION(MAX:MAXX,MAXY), REDUCTION(MIN:MINX,MINY)
        DO I = 2, C_POINT_NUM
            IF(C_POINT(1,I) < MINX) THEN
                MINX = C_POINT(1,I)
            ENDIF
            IF(C_POINT(1,I) > MAXX) THEN
                MAXX = C_POINT(1,I)
            ENDIF
            IF(C_POINT(2,I) < MINY) THEN
                MINY = C_POINT(2,I)
            ENDIF
            IF(C_POINT(2,I) > MAXY) THEN
                MAXY = C_POINT(2,I)
            ENDIF
            IF(C_POINT(3,I) < MINZ) THEN
                MINZ = C_POINT(3,I)
            ENDIF
            IF(C_POINT(3,I) > MAXZ) THEN
                MAXZ = C_POINT(3,I)
            ENDIF
        END DO
        !$OMP END PARALLEL DO
    
        DOMAIN_MAX(1) = MAXX + (MAXX-MINX)*0.1
        DOMAIN_MIN(1) = MINX - (MAXX-MINX)*0.1
        DOMAIN_MAX(2) = MAXY + (MAXY-MINY)*0.1
        DOMAIN_MIN(2) = MINY - (MAXY-MINY)*0.1
        DOMAIN_MAX(3) = MAXZ + (MAXZ-MINZ)*0.1
        DOMAIN_MIN(3) = MINZ - (MAXZ-MINZ)*0.1

	TEMP = MIN(DOMAIN_MAX(1)-DOMAIN_MIN(1),DOMAIN_MAX(2)-DOMAIN_MIN(2),DOMAIN_MAX(3)-DOMAIN_MIN(3))
	HASH_SIZE = TEMP/5.
        HASH_NUM1 = CEILING((DOMAIN_MAX(1)- DOMAIN_MIN(1))/HASH_SIZE)
        HASH_NUM2 = CEILING((DOMAIN_MAX(2)- DOMAIN_MIN(2))/HASH_SIZE)
        HASH_NUM3 = CEILING((DOMAIN_MAX(3)- DOMAIN_MIN(3))/HASH_SIZE)        
        
        CALL RESET_SURFACE(0, .TRUE., F_POINT_NUM, F_FACE_NUM, SURFACE_POINTS = F_POINT, SURFACE_FACES = F_FACE, &
        FACE_LOCATION = F_LOC)
        
        ALLOCATE(TEMP_FACE(3,P_FACE_NUM*2))
        ALLOCATE(TEMP_LOC(P_FACE_NUM*2))
        CALL GENERATE_FACES_CASE(P_POINT_NUM, P_POINT, P_FACE_NUM, P_FACE, TEMP_FACE)
        CALL GENERATE_LOCATION_CASE(P_FACE_NUM, P_LOC, TEMP_LOC)
        CALL RESET_SURFACE(1, .TRUE., P_POINT_NUM, 2*P_FACE_NUM, SURFACE_POINTS = P_POINT, SURFACE_FACES = TEMP_FACE, &
        FACE_LOCATION = TEMP_LOC)
        DEALLOCATE(TEMP_FACE)
        DEALLOCATE(TEMP_LOC)
        
        ALLOCATE(TEMP_FACE(3,C_FACE_NUM*2))
        CALL GENERATE_FACES_CASE(C_POINT_NUM, C_POINT, C_FACE_NUM, C_FACE, TEMP_FACE)
        CALL RESET_SURFACE(2, .TRUE., C_POINT_NUM, 2*C_FACE_NUM, SURFACE_POINTS = C_POINT, SURFACE_FACES = TEMP_FACE)
        DEALLOCATE(TEMP_FACE)
        
        CALL FIND_INTERFACE(0,2)
        CALL FIND_INTERFACE(1,2)
        CALL FIND_INTERFACE(2,2)
        
        CALL BOUNDARY_DIVIDING_TYP(0)
        CALL AREA_DIVIDING_TYP(0)
        
        CALL BOUNDARY_DIVIDING_TYP(1)
        CALL AREA_DIVIDING_TYP(1)
        
        CALL BOUNDARY_DIVIDING_TYP(2)
        CALL AREA_DIVIDING_TYP(2)
        
!	SURFACE_FLUID%INITIAL_POINT_TYPE = SURFACE_FLUID%POINT_TYPE
!	SURFACE_PROPEL%INITIAL_POINT_TYPE = SURFACE_PROPEL%POINT_TYPE
!	SURFACE_CASE%INITIAL_POINT_TYPE = SURFACE_CASE%POINT_TYPE
        
        CALL FIND_RELATEDPT(1,0,MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3)),1)
        CALL FIND_RELATEDPT(2,1,MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3)),1)
        
        CALL FIND_RELATEDFACE(1,0,1,.FALSE.)
        CALL FIND_RELATEDFACE(0,2,1,.FALSE.)
        CALL FIND_RELATEDFACE(1,2,1,.FALSE.)
!        CALL FIND_RELATEDFACE(2,0,1,.FALSE.)
        
        CALL FIND_IMPACT_ZONE(0,0)
        CALL FIND_IMPACT_ZONE(0,2)
        
        CALL FIND_IMPACT_ZONE(1,1)
        
        CALL FIND_INTERFACE_CLUSTER(0)
        CALL FIND_INTERFACE_CLUSTER(1)
        
        WRITE(*,*) 'SURFACE INITIALIZATION FINISHED'

	ELSE IF(RESTART_FLAG .EQ. 0) THEN
        	CALL INIT_RANDOM_SEED()
		CALL READ_RESTART(0)
		CALL READ_RESTART(1)
		CALL READ_RESTART(2)
                SURFACE_TOTAL_TIME = TIMESTEP * RESTART_ITERATION
		IF(DOMAIN_MAX(1) - DOMAIN_MIN(1) > DOMAIN_MAX(2) - DOMAIN_MIN(2) .AND. DOMAIN_MAX(1) - DOMAIN_MIN(1) > DOMAIN_MAX(3) - DOMAIN_MIN(3)) THEN
		    DOMAIN_MAX(2) = (DOMAIN_MAX(2) + DOMAIN_MIN(2))/2. + (DOMAIN_MAX(1) - DOMAIN_MIN(1))/2.
		    DOMAIN_MIN(2) = (DOMAIN_MAX(2) + DOMAIN_MIN(2))/2. - (DOMAIN_MAX(1) - DOMAIN_MIN(1))/2.
		    
		    DOMAIN_MAX(3) = (DOMAIN_MAX(3) + DOMAIN_MIN(3))/2. + (DOMAIN_MAX(1) - DOMAIN_MIN(1))/2.
		    DOMAIN_MIN(3) = (DOMAIN_MAX(3) + DOMAIN_MIN(3))/2. - (DOMAIN_MAX(1) - DOMAIN_MIN(1))/2.
		ELSE IF(DOMAIN_MAX(2) - DOMAIN_MIN(2) > DOMAIN_MAX(1) - DOMAIN_MIN(1) .AND. DOMAIN_MAX(2) - DOMAIN_MIN(2) > DOMAIN_MAX(3) - DOMAIN_MIN(3)) THEN
		    DOMAIN_MAX(1) = (DOMAIN_MAX(1) + DOMAIN_MIN(1))/2. + (DOMAIN_MAX(2) - DOMAIN_MIN(2))/2.
		    DOMAIN_MIN(1) = (DOMAIN_MAX(1) + DOMAIN_MIN(1))/2. - (DOMAIN_MAX(2) - DOMAIN_MIN(2))/2.
		    
		    DOMAIN_MAX(3) = (DOMAIN_MAX(3) + DOMAIN_MIN(3))/2. + (DOMAIN_MAX(2) - DOMAIN_MIN(2))/2.
		    DOMAIN_MIN(3) = (DOMAIN_MAX(3) + DOMAIN_MIN(3))/2. - (DOMAIN_MAX(2) - DOMAIN_MIN(2))/2.
		ELSE
		    DOMAIN_MAX(1) = (DOMAIN_MAX(1) + DOMAIN_MIN(1))/2. + (DOMAIN_MAX(3) - DOMAIN_MIN(3))/2.
		    DOMAIN_MIN(1) = (DOMAIN_MAX(1) + DOMAIN_MIN(1))/2. - (DOMAIN_MAX(3) - DOMAIN_MIN(3))/2.
		    
		    DOMAIN_MAX(2) = (DOMAIN_MAX(2) + DOMAIN_MIN(2))/2. + (DOMAIN_MAX(3) - DOMAIN_MIN(3))/2.
		    DOMAIN_MIN(2) = (DOMAIN_MAX(2) + DOMAIN_MIN(2))/2. - (DOMAIN_MAX(3) - DOMAIN_MIN(3))/2.
		END IF
	END IF
    END IF
        
        
    
    IF(TYPE1==0) THEN

        IF(TYPE2==0) THEN
                
            WRITE(*,*) 'FLUID SURFACE MOVING STARTED'

	    SURFACE_AREA_ITER = SURFACE_AREA_ITER + 1
            
	    !$OMP PARALLEL DO PRIVATE(I)
            DO I = 1, SURFACE_FLUID%SURFACE_FACES_NUM
                IF(SURFACE_FLUID%FACE_ONINTERFACE(I)==1) THEN ! .OR. SURFACE_FLUID%FACE_ABLATION_FLAG(I) == -2
		    IF(PRESENT(F_B_RATE)) THEN
		    SURFACE_FLUID%FACE_B_RATE(I) = F_B_RATE(I)
		    ELSE
                    SURFACE_FLUID%FACE_B_RATE(I) = 0.008
		    END IF
                ELSE
                    SURFACE_FLUID%FACE_B_RATE(I) = 0.
                END IF
            END DO
            !$OMP END PARALLEL DO

	    CALL COMPUTE_SURFACE_AREA(0,MODEL_ANGLE)
                
            CALL FLUID_MOVE(TIMESTEP)
            SURFACE_TOTAL_TIME = SURFACE_TOTAL_TIME + TIMESTEP
            
	    !$OMP PARALLEL DO PRIVATE(I)
            DO I=1, SURFACE_FLUID%SURFACE_POINTS_NUM
                F_POINT(1,I) = SURFACE_FLUID%SURFACE_POINTS(1,I) + SURFACE_FLUID%POINT_DISPLACEMENT(1,I)
                F_POINT(2,I) = SURFACE_FLUID%SURFACE_POINTS(2,I) + SURFACE_FLUID%POINT_DISPLACEMENT(2,I)
                F_POINT(3,I) = SURFACE_FLUID%SURFACE_POINTS(3,I) + SURFACE_FLUID%POINT_DISPLACEMENT(3,I)
            END DO
            !$OMP END PARALLEL DO
            
    	    !$OMP PARALLEL DO PRIVATE(I)
            DO I=1, SURFACE_PROPEL%SURFACE_POINTS_NUM
                P_POINT(1,I) = SURFACE_PROPEL%SURFACE_POINTS(1,I)
                P_POINT(2,I) = SURFACE_PROPEL%SURFACE_POINTS(2,I)
                P_POINT(3,I) = SURFACE_PROPEL%SURFACE_POINTS(3,I)
            END DO
	    !$OMP END PARALLEL DO
            
    	    !$OMP PARALLEL DO PRIVATE(I)
            DO I=1, SURFACE_CASE%SURFACE_POINTS_NUM
                C_POINT(1,I) = SURFACE_CASE%SURFACE_POINTS(1,I)
                C_POINT(2,I) = SURFACE_CASE%SURFACE_POINTS(2,I)
                C_POINT(3,I) = SURFACE_CASE%SURFACE_POINTS(3,I)
            END DO
	    !$OMP END PARALLEL DO

            WRITE(*,*) 'FLUID SURFACE MOVING ENDED'
            
        ELSE IF(TYPE2==1) THEN
            
            WRITE(*,*) 'FLUID SURFACE REMESHING STARTED'
            
            FLAG = .FALSE.
            
            !CALL ATTACH_FLUID_CASE_IMPACT_ZONE()
            !CALL ZIPPER_FLUID_IMPACT_ZONE(FLAG)
            
            ALLOCATE(TYP1_POINT(3,INTERFACE_FLUID_POINTS_NUM))
            ALLOCATE(TYP1_POINTLOC(3,INTERFACE_FLUID_POINTS_NUM))
            ALLOCATE(TYP1_FACE(3,INTERFACE_FLUID_FACES_NUM))
            ALLOCATE(DATA_DISPLACEMENT(3,SURFACE_FLUID%SURFACE_POINTS_NUM))
            
            TYP1_POINT_NUM = INTERFACE_FLUID_POINTS_NUM
            TYP1_POINT = INTERFACE_FLUID_POINTS
            TYP1_POINTLOC = INTERFACE_FLUID_POINTS_LOC
            TYP1_FACE_NUM = INTERFACE_FLUID_FACES_NUM
            TYP1_FACE = INTERFACE_FLUID_FACES
            DATA_DISPLACEMENT = SURFACE_FLUID%POINT_DISPLACEMENT
            
            CALL REMESHING_PROCESS(0, FLAG)

            IF(FLAG) THEN
                CALL FIND_RELATEDPT(1,0,MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3)),1)
                !CALL FIND_RELATEDPT(2,1,MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3)),1)
                
                CALL FIND_RELATEDFACE(0,2,1,.FALSE.)
		CALL FIND_RELATEDFACE(1,0,1,.FALSE.)
                CALL FIND_RELATEDFACE(1,2,1,.FALSE.)
!		CALL FIND_RELATEDFACE(2,0,1,.FALSE.)
                
                CALL FIND_IMPACT_ZONE(0,0)
                CALL FIND_IMPACT_ZONE(0,2)
                
!                CALL FIND_IMPACT_ZONE(1,1)

                CALL FIND_INTERFACE_CLUSTER(0)
!                CALL FIND_INTERFACE_CLUSTER(1)
                
                F_POINT_NUM = SURFACE_FLUID%SURFACE_POINTS_NUM
                DEALLOCATE(F_POINT)
                ALLOCATE(F_POINT(3,F_POINT_NUM))
                F_FACE_NUM = SURFACE_FLUID%SURFACE_FACES_NUM
                DEALLOCATE(F_FACE)
                ALLOCATE(F_FACE(3,F_FACE_NUM))
                
                CALL INTERPOLATE_FLUID_DISPLACEMENT(TYP1_POINT_NUM, TYP1_POINT, TYP1_POINTLOC, TYP1_FACE_NUM, TYP1_FACE, DATA_DISPLACEMENT)
                
		!$OMP PARALLEL DO PRIVATE(I)
                DO I=1, SURFACE_FLUID%SURFACE_POINTS_NUM
                    F_POINT(1,I) = SURFACE_FLUID%SURFACE_POINTS(1,I) + SURFACE_FLUID%POINT_DISPLACEMENT(1,I)
                    F_POINT(2,I) = SURFACE_FLUID%SURFACE_POINTS(2,I) + SURFACE_FLUID%POINT_DISPLACEMENT(2,I)
                    F_POINT(3,I) = SURFACE_FLUID%SURFACE_POINTS(3,I) + SURFACE_FLUID%POINT_DISPLACEMENT(3,I)
                END DO
		!$OMP END PARALLEL DO
                
		!$OMP PARALLEL DO PRIVATE(I)
                DO I=1, SURFACE_FLUID%SURFACE_FACES_NUM
                    F_FACE(1,I) = SURFACE_FLUID%SURFACE_FACES(1,I)
                    F_FACE(2,I) = SURFACE_FLUID%SURFACE_FACES(2,I)
                    F_FACE(3,I) = SURFACE_FLUID%SURFACE_FACES(3,I)
                END DO
        	!$OMP END PARALLEL DO
                
                DEALLOCATE(F_LOC)
                ALLOCATE(F_LOC(F_FACE_NUM))
                
                !$OMP PARALLEL DO PRIVATE(I)
                DO I=1, SURFACE_FLUID%SURFACE_FACES_NUM
                    F_LOC(I) = SURFACE_FLUID%FACE_LOCATION(I)
                END DO
		!$OMP END PARALLEL DO
            END IF

            DEALLOCATE(TYP1_POINT)
            DEALLOCATE(TYP1_POINTLOC)
            DEALLOCATE(TYP1_FACE)
            DEALLOCATE(DATA_DISPLACEMENT)

            WRITE(*,*) 'FLUID SURFACE REMESHING ENDED'

        END IF
            
            
    ELSE IF(TYPE1==1) THEN

        IF(TYPE2==0) THEN

            WRITE(*,*) 'STRUCT SURFACE MOVING STARTED'
            
            !$OMP PARALLEL DO PRIVATE(I)
            DO I = 1, SURFACE_PROPEL%SURFACE_POINTS_NUM
                SURFACE_PROPEL%POINT_VELOCITY(:,I) = P_DISPLACEMENT(:,I) - SURFACE_PROPEL%POINT_DISPLACEMENT(:,I)
                SURFACE_PROPEL%POINT_DISPLACEMENT(:,I) = P_DISPLACEMENT(:,I)
            END DO
            !$OMP END PARALLEL DO
            
	    !$OMP PARALLEL DO PRIVATE(I)
            DO I = 1, SURFACE_CASE%SURFACE_POINTS_NUM
                SURFACE_CASE%POINT_VELOCITY(:,I) = C_DISPLACEMENT(:,I) - SURFACE_CASE%POINT_DISPLACEMENT(:,I)
                SURFACE_CASE%POINT_DISPLACEMENT(:,I) = C_DISPLACEMENT(:,I)
            END DO
	    !$OMP END PARALLEL DO
            
            CALL STRUCT_MOVE()
            
	    !$OMP PARALLEL DO PRIVATE(I)
            DO I=1, SURFACE_PROPEL%SURFACE_POINTS_NUM
                P_POINT(1,I) = SURFACE_PROPEL%SURFACE_POINTS(1,I)
                P_POINT(2,I) = SURFACE_PROPEL%SURFACE_POINTS(2,I)
                P_POINT(3,I) = SURFACE_PROPEL%SURFACE_POINTS(3,I)
            END DO
            !$OMP END PARALLEL DO
            
	    !$OMP PARALLEL DO PRIVATE(I)
            DO I=1, SURFACE_CASE%SURFACE_POINTS_NUM
                C_POINT(1,I) = SURFACE_CASE%SURFACE_POINTS(1,I)
                C_POINT(2,I) = SURFACE_CASE%SURFACE_POINTS(2,I)
                C_POINT(3,I) = SURFACE_CASE%SURFACE_POINTS(3,I)
            END DO
            !$OMP END PARALLEL DO
            
	    !$OMP PARALLEL DO PRIVATE(I)
            DO I=1, SURFACE_FLUID%SURFACE_POINTS_NUM
                F_POINT(1,I) = SURFACE_FLUID%SURFACE_POINTS(1,I) + SURFACE_FLUID%POINT_DISPLACEMENT(1,I)
                F_POINT(2,I) = SURFACE_FLUID%SURFACE_POINTS(2,I) + SURFACE_FLUID%POINT_DISPLACEMENT(2,I)
                F_POINT(3,I) = SURFACE_FLUID%SURFACE_POINTS(3,I) + SURFACE_FLUID%POINT_DISPLACEMENT(3,I)
            END DO
            !$OMP END PARALLEL DO
            
            WRITE(*,*) 'STRUCT SURFACE MOVING ENDED'
            
        ELSE IF(TYPE2==1) THEN
            
            WRITE(*,*) 'PROPEL SURFACE REMESHING STARTED'
            CALL RESET_PATCH(1)

	    SURFACE_PRESSURE_ITER = SURFACE_PRESSURE_ITER + 1
	    FLAG = .FALSE.
            
            !CALL ZIPPER_PROPEL_IMPACT_ZONE(FLAG)
            TEMPFLAG = .FALSE.
            IF(.NOT. FLAG) THEN
	        DO I=1,SURFACE_PROPEL%SURFACE_PATCHES_NUM
		    IF(SURFACE_PROPEL%SURFACE_PATCHES_TOPCHANGE_TYP(I) .EQ. 11) THEN
		       TEMPFLAG = .TRUE.
		    END IF
	        END DO
            END IF
            
            IF(.NOT. TEMPFLAG) THEN
                CALL MESH_QUALITY_PROCESS(1, FLAG, FLAG_ARRAY = FLAG_ARRAY)
		IF(.NOT. FLAG) THEN
	              SURFACE_FLAG_ARRAY(SURFACE_PRESSURE_ITER,1:SURFACE_PROPEL%SURFACE_PATCHES_NUM) = FLAG_ARRAY
                END IF
                FLAG = FLAG .OR. TEMPFLAG
            END IF
            
            
            IF(FLAG) THEN

		DO I=1,SURFACE_PROPEL%SURFACE_PATCHES_NUM
		   IF(SURFACE_PROPEL%SURFACE_PATCHES_TOPCHANGE_TYP(I) .GE. 2) THEN
	               FLAG_ARRAY(I) = SURFACE_PROPEL%SURFACE_PATCHES_TOPCHANGE_TYP(I)
		   END IF
		       SURFACE_FLAG_ARRAY(SURFACE_PRESSURE_ITER,I) = FLAG_ARRAY(I)
		END DO
                
                CALL RESET_DIVIDED_REGION_BOUNDARY(SURFACE_PROPEL%SURFACE_POINTS_NUM, SURFACE_PROPEL%SURFACE_POINTS, SURFACE_PROPEL%SURFACE_FACES_NUM, SURFACE_PROPEL%SURFACE_FACES, SURFACE_PROPEL%POINT_FACE_CONNECTION_NUM, SURFACE_PROPEL%POINT_FACE_CONNECTION, SURFACE_PROPEL%POINT_TYPE, SURFACE_PROPEL%FACE_DIVIDED_REGION_ARRAY, SURFACE_PROPEL%FACE_DIVIDED_REGION_NUM, SURFACE_PROPEL%FACE_DIVIDED_BOUNDARY_ARRAY, SURFACE_PROPEL%FACE_DIVIDED_BOUNDARY_NUM)
                
                !DO I=1,SURFACE_PROPEL%FACE_DIVIDED_BOUNDARY_ARRAY
                !END DO
                
                P_POINT_NUM = SURFACE_PROPEL%SURFACE_POINTS_NUM
                DEALLOCATE(P_POINT)
                ALLOCATE(P_POINT(3,P_POINT_NUM))
                P_FACE_NUM = SURFACE_PROPEL%SURFACE_FACES_NUM
                DEALLOCATE(P_FACE)
                ALLOCATE(P_FACE(3,P_FACE_NUM))
                
		!$OMP PARALLEL DO PRIVATE(I)
                DO I=1, SURFACE_PROPEL%SURFACE_POINTS_NUM
                    P_POINT(1,I) = SURFACE_PROPEL%SURFACE_POINTS(1,I)
                    P_POINT(2,I) = SURFACE_PROPEL%SURFACE_POINTS(2,I)
                    P_POINT(3,I) = SURFACE_PROPEL%SURFACE_POINTS(3,I)
                END DO
		!$OMP END PARALLEL DO
                
		!$OMP PARALLEL DO PRIVATE(I)
                DO I=1, SURFACE_PROPEL%SURFACE_FACES_NUM
                    P_FACE(1,I) = SURFACE_PROPEL%SURFACE_FACES(1,I)
                    P_FACE(2,I) = SURFACE_PROPEL%SURFACE_FACES(2,I)
                    P_FACE(3,I) = SURFACE_PROPEL%SURFACE_FACES(3,I)
                END DO
		!$OMP END PARALLEL DO
                
                DEALLOCATE(P_LOC)
                ALLOCATE(P_LOC(P_FACE_NUM))
                
		!$OMP PARALLEL DO PRIVATE(I)
                DO I=1, SURFACE_PROPEL%SURFACE_FACES_NUM
                    P_LOC(I) = SURFACE_PROPEL%FACE_LOCATION(I)
                END DO
		!$OMP END PARALLEL DO
                
                !DEALLOCATE(P_FACE_AREA)
                !ALLOCATE(P_FACE_AREA(P_FACE_NUM))
                !DEALLOCATE(P_CORNER_INDEX)
                !ALLOCATE(P_CORNER_INDEX(P_POINT_NUM))
                
                !CALL SAVING_COEFFS_SURFACE_STRUCT(P_FACE_AREA, P_CORNER_INDEX)
            END IF

            WRITE(*,*) 'PROPEL SURFACE REMESHING ENDED'
            
        ELSE IF(TYPE2==2) THEN
            
            WRITE(*,*) 'CASE SURFACE REMESHING STARTED'
            
	    FLAG = .FALSE.
            
            CALL MESH_QUALITY_PROCESS(2, FLAG)
            
            WRITE(*,*) 'CASE SURFACE REMESHING ENDED'
        END IF
        
    ELSE IF(TYPE1 == 2) THEN
        !IF(TYPE2 == 1) THEN
        
        WRITE(*,*) 'PROPEL SURFACE RESET STARTED'
        
        ALLOCATE(TEMP_ONINTERFACE(P_FACE_NUM))
        ALLOCATE(TEMP_CONNECTION_NUM(P_POINT_NUM))
        ALLOCATE(TEMP_CONNECTION(30,P_POINT_NUM))
        
        ALLOCATE(TEMP_FACE(3,P_FACE_NUM*2))
        ALLOCATE(TEMP_LOC(P_FACE_NUM*2))
        CALL GENERATE_FACES_CASE(P_POINT_NUM, P_POINT, P_FACE_NUM, P_FACE, TEMP_FACE)
        CALL GENERATE_LOCATION_CASE(P_FACE_NUM, P_LOC, TEMP_LOC)
        
        CALL LOADING_COEFFS_SURFACE_STRUCT(P_POINT_NUM, P_POINT, 2*P_FACE_NUM, TEMP_FACE, TEMP_LOC, TEMP_ONINTERFACE, TEMP_CONNECTION_NUM, TEMP_CONNECTION)
        CALL RESET_SURFACE(1, .TRUE., P_POINT_NUM, 2*P_FACE_NUM, SURFACE_POINTS = P_POINT, SURFACE_FACES = TEMP_FACE, POINT_FACE_CONNECTION_NUM = TEMP_CONNECTION_NUM, POINT_FACE_CONNECTION = TEMP_CONNECTION, FACE_LOCATION = TEMP_LOC)

        DEALLOCATE(TEMP_FACE)
        DEALLOCATE(TEMP_LOC)
        
        DEALLOCATE(TEMP_ONINTERFACE)
        DEALLOCATE(TEMP_CONNECTION_NUM)
        DEALLOCATE(TEMP_CONNECTION)
        
        CALL FIND_INTERFACE(1,2)
        
        CALL BOUNDARY_DIVIDING_TYP(1)
        CALL AREA_DIVIDING_TYP(1)
        
!	SURFACE_PROPEL%INITIAL_POINT_TYPE = SURFACE_PROPEL%POINT_TYPE
        
        CALL FIND_RELATEDPT(1,0,MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3)),1)
!        CALL FIND_RELATEDPT(2,1,MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3)),1)
        
!        CALL FIND_RELATEDFACE(0,2,1,.FALSE.)
        CALL FIND_RELATEDFACE(1,0,1,.FALSE.)
!        CALL FIND_RELATEDFACE(1,2,1,.FALSE.)
!        CALL FIND_RELATEDFACE(2,0,1,.FALSE.)
        
!        CALL FIND_IMPACT_ZONE(0,0)
!        CALL FIND_IMPACT_ZONE(0,2)
        CALL FIND_IMPACT_ZONE(1,1)
        
!        CALL FIND_INTERFACE_CLUSTER(0)
!        CALL FIND_INTERFACE_CLUSTER(1)

        WRITE(*,*) 'PROPEL SURFACE RESET ENDED'
        
        !ELSE IF(TYPE2==2) THEN
        
        WRITE(*,*) 'CASE SURFACE RESET STARTED'
        
        ALLOCATE(TEMP_ONINTERFACE2(C_FACE_NUM))
        ALLOCATE(TEMP_CONNECTION_NUM2(C_POINT_NUM))
        ALLOCATE(TEMP_CONNECTION2(30,C_POINT_NUM))
        
        ALLOCATE(TEMP_FACE2(3,C_FACE_NUM*2))
        ALLOCATE(TEMP_LOC2(C_FACE_NUM*2))
        
        CALL GENERATE_FACES_CASE(C_POINT_NUM, C_POINT, C_FACE_NUM, C_FACE, TEMP_FACE2)
        
	CALL LOADING_COEFFS_SURFACE_STRUCT(C_POINT_NUM, C_POINT, 2*C_FACE_NUM, TEMP_FACE2, TEMP_LOC2, TEMP_ONINTERFACE2, TEMP_CONNECTION_NUM2, TEMP_CONNECTION2)
        CALL RESET_SURFACE(2, .TRUE., C_POINT_NUM, 2*C_FACE_NUM, SURFACE_POINTS = C_POINT, SURFACE_FACES = TEMP_FACE2) !POINT_FACE_CONNECTION_NUM = TEMP_CONNECTION_NUM2, POINT_FACE_CONNECTION = TEMP_CONNECTION2)
        
        DEALLOCATE(TEMP_FACE2)
        
        DEALLOCATE(TEMP_ONINTERFACE2)
        DEALLOCATE(TEMP_CONNECTION_NUM2)
        DEALLOCATE(TEMP_CONNECTION2)
	DEALLOCATE(TEMP_LOC2)
        
        CALL FIND_INTERFACE(2,2)
        
        CALL BOUNDARY_DIVIDING_TYP(2)
        CALL AREA_DIVIDING_TYP(2)

!	SURFACE_CASE%INITIAL_POINT_TYPE = SURFACE_CASE%POINT_TYPE
        
        
!        CALL FIND_RELATEDPT(1,0,MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3)),1)
        CALL FIND_RELATEDPT(2,1,MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3)),1)
        
        CALL FIND_RELATEDFACE(0,2,1,.FALSE.)
!        CALL FIND_RELATEDFACE(1,0,1,.FALSE.)
        CALL FIND_RELATEDFACE(1,2,1,.FALSE.)
        CALL FIND_RELATEDFACE(2,0,1,.FALSE.)
        
!        CALL FIND_IMPACT_ZONE(0,0)
        CALL FIND_IMPACT_ZONE(0,2)
!        CALL FIND_IMPACT_ZONE(1,1)
        
!        CALL FIND_INTERFACE_CLUSTER(0)
        CALL FIND_INTERFACE_CLUSTER(1)
        
        WRITE(*,*) 'CASE SURFACE RESET ENDED'
        
        !END IF
    ELSE IF(TYPE1==3) THEN

        WRITE(*,*) 'PRESSURE TRANSFER STARTED'

        !$OMP PARALLEL DO PRIVATE(I)
        DO I = 1, SURFACE_PROPEL%SURFACE_POINTS_NUM
           SURFACE_PROPEL%POINT_DISPLACEMENT(:,I) = P_DISPLACEMENT(:,I)
        END DO
        !$OMP END PARALLEL DO

	!$OMP PARALLEL DO PRIVATE(I)
        DO I = 1, SURFACE_CASE%SURFACE_POINTS_NUM
            SURFACE_CASE%POINT_DISPLACEMENT(:,I) = C_DISPLACEMENT(:,I)
        END DO
	!$OMP END PARALLEL DO
        
	!$OMP PARALLEL DO PRIVATE(I)
        DO I = 1, SURFACE_FLUID%SURFACE_FACES_NUM
            SURFACE_FLUID%FACE_PRESSURE(I) = F_PRESSURE(I)
        END DO
        !$END OMP PARALLEL DO
        
        CALL PRESSURE_TRANSFER()
        
	!$OMP PARALLEL DO PRIVATE(I)
        DO I = 1, SURFACE_PROPEL%SURFACE_POINTS_NUM
            P_FORCE(1,I) = SURFACE_PROPEL%POINT_FORCE(1,I)
            P_FORCE(2,I) = SURFACE_PROPEL%POINT_FORCE(2,I)
            P_FORCE(3,I) = SURFACE_PROPEL%POINT_FORCE(3,I)
        END DO
        !$END OMP PARALLEL DO
        
	!$OMP PARALLEL DO PRIVATE(I)
        DO I = 1, SURFACE_CASE%SURFACE_POINTS_NUM
            C_FORCE(1,I) = SURFACE_CASE%POINT_FORCE(1,I)
            C_FORCE(2,I) = SURFACE_CASE%POINT_FORCE(2,I)
            C_FORCE(3,I) = SURFACE_CASE%POINT_FORCE(3,I)
        END DO
        !$END OMP PARALLEL DO

        CALL SAVINGINTERFACE_TECPLOT(99999)
        CALL SAVINGDATA_TECPLOT(99999)
        
        WRITE(*,*) 'PRESSURE TRANSFER ENDED'

    ELSE IF(TYPE1==50) THEN
    
        WRITE(*,*) 'REMESHING TEST'
        
        FLAG = .FALSE.
        
        CALL SAVINGDATA_TECPLOT(500)
        
        CALL REMESHING_PROCESS(0, FLAG)
        
        CALL SAVINGDATA_TECPLOT(501)

    ELSE IF(TYPE1==100) THEN

        !CALL SAVINGPOINT(FILENUM,0)
        !CALL SAVINGFACE(FILENUM,0)
        !CALL SAVINGPOINT(FILENUM,1)
        !CALL SAVINGFACE(FILENUM,1)
        !CALL SAVINGPOINT(FILENUM,2)
        !CALL SAVINGFACE(FILENUM,2)
        
        CALL SAVINGDATA_TECPLOT(FILENUM)
        
    ELSE IF(TYPE1==101) THEN

        !CALL SAVINGDISPLACEMENT(FILENUM,0)
        !CALL SAVINGDISPLACEMENT(FILENUM,1)
        !CALL SAVINGDISPLACEMENT(FILENUM,2)

    ELSE IF(TYPE1==102) THEN

        !CALL SAVINGBRATE(FILENUM,0)
        !CALL SAVINGBRATE(FILENUM,1)
        !CALL SAVINGBRATE(FILENUM,2)

    ELSE IF(TYPE1==103) THEN

        !CALL SAVINGPRESSURE(FILENUM,0)
        !CALL SAVINGPRESSURE(FILENUM,1)
        !CALL SAVINGPRESSURE(FILENUM,2)
    
    ELSE IF(TYPE1==104) THEN
        CALL SAVINGINTERFACE_TECPLOT(FILENUM)

    ELSE IF(TYPE1==105) THEN
	!CALL SAVING_SURFACE(FILENUM,0)
	!CALL SAVING_SURFACE(FILENUM,1)
	!CALL SAVING_SURFACE(FILENUM,2)
    END IF
    
END SUBROUTINE SURFACE_3D

