MODULE OPERATORS_3D

    USE SURFACE_MODULE_3D
    USE SURFACES_3D
    USE SVD
    USE HASHING_3D

    IMPLICIT NONE
    
    CONTAINS
    

    SUBROUTINE DISTANCE_SURFACE_POINT_TYPE_ITER(N1, CURRENT_HASH, RET)
        IMPLICIT NONE
        INTEGER :: N1, N2
        TYPE(HASH), POINTER :: CURRENT_HASH
        INTEGER :: I, I1, I2, I3
        REAL(8) :: RET, TEMPDIST
        REAL(8) :: V(3), V1(3), V2(3), V3(3)
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT2
        
        IF (CURRENT_HASH%TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (CURRENT_HASH%TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
        
        V = SURFACE_CURRENT1%SURFACE_POINTS(:,N1)
        
        DO N2 = 1, CURRENT_HASH%ELEMENT_NUM2
            I = CURRENT_HASH%ELEMENT2(N2)
            I1 = SURFACE_CURRENT2%SURFACE_FACES(1,I)
            I2 = SURFACE_CURRENT2%SURFACE_FACES(2,I)
            I3 = SURFACE_CURRENT2%SURFACE_FACES(3,I)
            V1 = SURFACE_CURRENT2%SURFACE_POINTS(:,I1)
            V2 = SURFACE_CURRENT2%SURFACE_POINTS(:,I2)
            V3 = SURFACE_CURRENT2%SURFACE_POINTS(:,I3)
            CALL UNSIGNED_DISTANCE_FACE_POINT(V, V1, V2, V3, TEMPDIST)

            IF (TEMPDIST <= RET) THEN
                RET = TEMPDIST
            END IF
        END DO
        
        NULLIFY(SURFACE_CURRENT1)
        NULLIFY(SURFACE_CURRENT2)
    END SUBROUTINE

    
    SUBROUTINE DISTANCE_SURFACE_POINT_TYPE(N1,CURRENT_HASH,RET)
        IMPLICIT NONE
        INTEGER :: N1
        TYPE(HASH), POINTER :: CURRENT_HASH
        TYPE(HASH), POINTER :: NBHD_HASH
        REAL(8) :: RET
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT2
        
        IF (CURRENT_HASH%TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (CURRENT_HASH%TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
        
        RET = 100

        NBHD_HASH => CURRENT_HASH
        CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
                   
        NBHD_HASH => CURRENT_HASH%LEFT
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%RIGHT
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%BOTTOM
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%TOP
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%BACK
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%FRONT
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
        END IF
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	    IF (ASSOCIATED(CURRENT_HASH%LEFT)) THEN              
            NBHD_HASH => CURRENT_HASH%LEFT%BACK
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF

      	    IF (ASSOCIATED(CURRENT_HASH%LEFT%BACK)) THEN 
                NBHD_HASH => CURRENT_HASH%LEFT%BACK%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%LEFT%BACK%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
        END IF
                    
            NBHD_HASH => CURRENT_HASH%LEFT%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%LEFT%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%LEFT%FRONT
            IF (ASSOCIATED(NBHD_HASH)) THEN
               CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            IF (ASSOCIATED(CURRENT_HASH%LEFT%FRONT)) THEN
                NBHD_HASH => CURRENT_HASH%LEFT%FRONT%TOP
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
                END IF
                    
                NBHD_HASH => CURRENT_HASH%LEFT%FRONT%BOTTOM
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
                END IF
	        END IF
	    END IF
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	    IF (ASSOCIATED(CURRENT_HASH%RIGHT)) THEN 
            NBHD_HASH => CURRENT_HASH%RIGHT%BACK
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            IF (ASSOCIATED(CURRENT_HASH%RIGHT%BACK)) THEN
            NBHD_HASH => CURRENT_HASH%RIGHT%BACK%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%RIGHT%BACK%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
        END IF
                    
            NBHD_HASH => CURRENT_HASH%RIGHT%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%RIGHT%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%RIGHT%FRONT
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
       
            IF (ASSOCIATED(CURRENT_HASH%RIGHT%FRONT)) THEN             
                NBHD_HASH => CURRENT_HASH%RIGHT%FRONT%TOP
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
                END IF
                    
                NBHD_HASH => CURRENT_HASH%RIGHT%FRONT%BOTTOM
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
                END IF
	        END IF
	    END IF
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	    IF (ASSOCIATED(CURRENT_HASH%BACK)) THEN
            NBHD_HASH => CURRENT_HASH%BACK%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%BACK%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
        END IF

	    IF (ASSOCIATED(CURRENT_HASH%FRONT)) THEN
            NBHD_HASH => CURRENT_HASH%FRONT%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%FRONT%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_POINT_TYPE_ITER(N1, NBHD_HASH, RET)
            END IF
        END IF
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        NULLIFY(NBHD_HASH)
        NULLIFY(SURFACE_CURRENT1)
        NULLIFY(SURFACE_CURRENT2)
    END SUBROUTINE DISTANCE_SURFACE_POINT_TYPE

    
    
    SUBROUTINE DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,CURRENT_HASH,  R, IDX, THRESH)
        IMPLICIT NONE
        TYPE(HASH), POINTER :: CURRENT_HASH
        INTEGER :: I0, N2, IDX
        INTEGER :: TYP1, TYP2
        REAL(8) :: R, V(3), L(3), T, THRESH
        INTEGER :: I, SGN
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT2
        
        
        TYP1 = CURRENT_HASH%TYP1
        TYP2 = CURRENT_HASH%TYP2
        
        IF (CURRENT_HASH%TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (CURRENT_HASH%TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
        
        DO N2 = 1, CURRENT_HASH%ELEMENT_NUM2
            I = CURRENT_HASH%ELEMENT2(N2)
            IF(TYP1==TYP2 .AND. I0==I) THEN
            ELSE
                CALL LINE_FACE_INTERSECTING(SURFACE_CURRENT2%SURFACE_POINTS_NUM, SURFACE_CURRENT2%SURFACE_POINTS, SURFACE_CURRENT2%SURFACE_FACES_NUM, SURFACE_CURRENT2%SURFACE_FACES,I,L,V,SGN,T)
                
                IF (SGN.NE.-2 .AND. SGN.NE.-3 .AND. T > -1.*THRESH .AND. ABS(T) < ABS(R)) THEN
                    R = T
                    IDX = I
                END IF
            END IF
        END DO
        
        NULLIFY(SURFACE_CURRENT1)
        NULLIFY(SURFACE_CURRENT2)
    END SUBROUTINE
    
    
    SUBROUTINE DISTANCE_SURFACE_FACE_TYPE(N1,CURRENT_HASH,  DIR, R, IDX)
        IMPLICIT NONE
        TYPE(HASH), POINTER :: CURRENT_HASH
        TYPE(HASH), POINTER :: NBHD_HASH
        INTEGER :: I0, N1, DIR, IDX
        REAL(8) :: THRESH
        REAL(8) :: R, V1(3), V2(3), V3(3), V(3), W1(3), W2(3), L(3), ERR
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT2
        
        
        IF (CURRENT_HASH%TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (CURRENT_HASH%TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
        
        R = 100
        IDX = 0
        I0 = CURRENT_HASH%ELEMENT1(N1)

        V1 = SURFACE_CURRENT1%SURFACE_POINTS(:,SURFACE_CURRENT1%SURFACE_FACES(1,I0))
        V2 = SURFACE_CURRENT1%SURFACE_POINTS(:,SURFACE_CURRENT1%SURFACE_FACES(2,I0))
        V3 = SURFACE_CURRENT1%SURFACE_POINTS(:,SURFACE_CURRENT1%SURFACE_FACES(3,I0))
        
        THRESH = (SQRT(DOT_PRODUCT(V2-V1,V2-V1)) + SQRT(DOT_PRODUCT(V3-V2,V3-V2)) + SQRT(DOT_PRODUCT(V3-V1,V3-V1)))/3.
        
        CALL RANDOM_NUMBER(ERR)
        V = (1./3. - 0.000001 * ERR) * V1 + (1./3. + 0.000001 * ERR) * V2 + 1./3. * V3
        W1 = V2-V1
        W2 = V3-V1
        CALL VEC_CURL1(W1, W2, L)
        
        L = L/SQRT(DOT_PRODUCT(L,L))
        
        IF(DIR==0) THEN
            L = -L
        END IF
        
        NBHD_HASH => CURRENT_HASH
        CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
        
        NBHD_HASH => CURRENT_HASH%LEFT
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%RIGHT
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%BOTTOM
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%TOP
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%BACK
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
        END IF
                    
        NBHD_HASH => CURRENT_HASH%FRONT
        IF (ASSOCIATED(NBHD_HASH)) THEN
            CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
        END IF
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
        IF (ASSOCIATED(CURRENT_HASH%LEFT)) THEN       
            NBHD_HASH => CURRENT_HASH%LEFT%BACK
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF

            IF (ASSOCIATED(CURRENT_HASH%LEFT%BACK)) THEN                    
                NBHD_HASH => CURRENT_HASH%LEFT%BACK%TOP
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
                END IF
                    
                NBHD_HASH => CURRENT_HASH%LEFT%BACK%BOTTOM
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
                END IF
            END IF
                    
            NBHD_HASH => CURRENT_HASH%LEFT%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%LEFT%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%LEFT%FRONT
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            IF (ASSOCIATED(CURRENT_HASH%LEFT%FRONT)) THEN
                NBHD_HASH => CURRENT_HASH%LEFT%FRONT%TOP
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
                END IF
                    
                NBHD_HASH => CURRENT_HASH%LEFT%FRONT%BOTTOM
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
                END IF
            END IF
        END IF
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (ASSOCIATED(CURRENT_HASH%RIGHT)) THEN
            NBHD_HASH => CURRENT_HASH%RIGHT%BACK
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            IF (ASSOCIATED(CURRENT_HASH%RIGHT%BACK)) THEN
                NBHD_HASH => CURRENT_HASH%RIGHT%BACK%TOP
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
                END IF
                    
                NBHD_HASH => CURRENT_HASH%RIGHT%BACK%BOTTOM
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
                END IF
            END IF
                    
            NBHD_HASH => CURRENT_HASH%RIGHT%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%RIGHT%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%RIGHT%FRONT
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            IF (ASSOCIATED(CURRENT_HASH%RIGHT%FRONT)) THEN
                NBHD_HASH => CURRENT_HASH%RIGHT%FRONT%TOP
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
                END IF
                    
                NBHD_HASH => CURRENT_HASH%RIGHT%FRONT%BOTTOM
                IF (ASSOCIATED(NBHD_HASH)) THEN
                    CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
                END IF
            END IF
        END IF
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (ASSOCIATED(CURRENT_HASH%BACK)) THEN
            NBHD_HASH => CURRENT_HASH%BACK%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%BACK%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
        END IF

        IF (ASSOCIATED(CURRENT_HASH%FRONT)) THEN                    
            NBHD_HASH => CURRENT_HASH%FRONT%TOP
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
                    
            NBHD_HASH => CURRENT_HASH%FRONT%BOTTOM
            IF (ASSOCIATED(NBHD_HASH)) THEN
                CALL DISTANCE_SURFACE_FACE_TYPE_ITER(L,V, I0,NBHD_HASH, R, IDX, THRESH)
            END IF
        END IF
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        NULLIFY(NBHD_HASH)
        NULLIFY(SURFACE_CURRENT1)
        NULLIFY(SURFACE_CURRENT2)
    END SUBROUTINE DISTANCE_SURFACE_FACE_TYPE
    
    SUBROUTINE DISTANCE_FACE_FACE_TYPE(I0,TYP1,J0,TYP2,DIR,  R,B, WEIGHT1, WEIGHT2, WEIGHT3)
        IMPLICIT NONE
        INTEGER :: TYP1, TYP2
        INTEGER :: I0, J0, DIR
        LOGICAL :: B
        REAL(8) :: R
        REAL(8), OPTIONAL :: WEIGHT1, WEIGHT2, WEIGHT3
        
        !INTEGER :: N1
        REAL(8) :: V1(3), V2(3), V3(3), V(3), W1(3), W2(3), L(3), ERR
        INTEGER :: SGN
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT2
        
        IF (TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
        
        IF(TYP1==TYP2 .AND. I0==J0) THEN
            R = 0.
            B = .FALSE.
            RETURN
        END IF

        V1 = SURFACE_CURRENT1%SURFACE_POINTS(:,SURFACE_CURRENT1%SURFACE_FACES(1,I0))
        V2 = SURFACE_CURRENT1%SURFACE_POINTS(:,SURFACE_CURRENT1%SURFACE_FACES(2,I0))
        V3 = SURFACE_CURRENT1%SURFACE_POINTS(:,SURFACE_CURRENT1%SURFACE_FACES(3,I0))
        
        IF(PRESENT(WEIGHT1)) THEN
            V = WEIGHT1 * V1 + WEIGHT2 * V2 + WEIGHT3 * V3
        ELSE
            CALL RANDOM_NUMBER(ERR)
            V = (1./3. - 0.000001 * ERR) * V1 + (1./3. + 0.000001 * ERR) * V2 + 1./3. * V3
        END IF
        
        W1 = V2-V1
        W2 = V3-V1
        CALL VEC_CURL1(W1, W2, L)
        
        L = L/SQRT(DOT_PRODUCT(L,L))
        
        IF(DIR==0) THEN
            L = -L
        END IF
        
        CALL LINE_FACE_INTERSECTING(SURFACE_CURRENT2%SURFACE_POINTS_NUM, SURFACE_CURRENT2%SURFACE_POINTS, SURFACE_CURRENT2%SURFACE_FACES_NUM, SURFACE_CURRENT2%SURFACE_FACES,J0,L,V,SGN,R)
        
        IF (SGN.NE.-2 .AND. SGN.NE.-3 .AND. R > -1.*(SQRT(DOT_PRODUCT(V2-V1,V2-V1)) + SQRT(DOT_PRODUCT(V3-V2,V3-V2)) + SQRT(DOT_PRODUCT(V3-V1,V3-V1)))/3.) THEN
            B = .TRUE.
        ELSE
            B = .FALSE.
        END IF
        
        NULLIFY(SURFACE_CURRENT1)
        NULLIFY(SURFACE_CURRENT2)
    END SUBROUTINE DISTANCE_FACE_FACE_TYPE
    
    SUBROUTINE FIND_IMPACT_ZONE(TYP1, TYP2)
        IMPLICIT NONE
        INTEGER :: TYP1, TYP2
        
        INTEGER :: I, IDX, DIR, J, K, I1  !, J1, J2, J11, J22 
	!integer :: J111, J222
        REAL(8) :: R 
	!real(8) :: RMIN
        TYPE(HASH), POINTER :: HASH0, TEMP_HASH0
        REAL(8) :: HASH_SIZE
        INTEGER :: HASH_NUM1, HASH_NUM2, HASH_NUM3, N1
        
        LOGICAL :: B
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        
        IF (TYP1==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        END IF
        IF (TYP1==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        END IF
        IF (TYP1==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF
        
        IF(TYP1==TYP2) THEN
            DIR = 0
        ELSE
            DIR = 1
        END IF
        
        SURFACE_CURRENT%FACE_IMPACT_ZONE(TYP2+1, :) = 0
        
        HASH_SIZE = 0.03
        CALL SETTING_HASH(HASH_SIZE, HASH_NUM1, HASH_NUM2, HASH_NUM3)
        NULLIFY(HASH0)
        CALL GENERATE_HASH(TYP1,2,TYP2,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH0)
        
        TEMP_HASH0 => HASH0
        DO WHILE (ASSOCIATED(TEMP_HASH0))
            DO N1 = 1, TEMP_HASH0%ELEMENT_NUM1
                I = TEMP_HASH0%ELEMENT1(N1)
                
                CALL DISTANCE_SURFACE_FACE_TYPE(N1,TEMP_HASH0,  DIR, R,IDX)
                
                B = .TRUE.
                
                DO J=1,3
                    DO K=1,SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT%SURFACE_FACES(J,I))
                        I1 = SURFACE_CURRENT%POINT_FACE_CONNECTION(K,SURFACE_CURRENT%SURFACE_FACES(J,I))
                        IF(IDX==I1) THEN
                            B = .FALSE.
                        END IF
                    END DO
                END DO
                
                IF(B) THEN
                    SURFACE_CURRENT%FACE_IMPACT_ZONE(TYP2+1, I) = IDX
                    !WRITE(*,*) 'minimum impact distance : ', RMIN
                END IF
                !IF(TYP1==TYP2 .AND. IDX==0) THEN
                !    WRITE(*,*) 'ERROR'
                !END IF
            END DO
            
            TEMP_HASH0=>TEMP_HASH0%NEXT
        END DO
	
	NULLIFY(TEMP_HASH0)
	CALL DELETE_HASH(HASH0)
        
    END SUBROUTINE FIND_IMPACT_ZONE

    
    SUBROUTINE UPDATE_IMPACT_ZONE(TYP1, TYP2)
        IMPLICIT NONE
        INTEGER :: TYP1, TYP2
        
        INTEGER :: I, J, K, DIR, I1, IPZ
        REAL(8) :: R
        LOGICAL :: B, FLAG
        
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1, SURFACE_CURRENT2
        
        IF (TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
        
        IF(TYP1==TYP2) THEN
            DIR = 0
        ELSE
            DIR = 1
        END IF
        
        DO I=1,SURFACE_CURRENT1%SURFACE_FACES_NUM
            IPZ = SURFACE_CURRENT1%FACE_IMPACT_ZONE(TYP2+1, I)
            
            FLAG = .FALSE.
            IF(IPZ==0) THEN
                FLAG = .TRUE.
            ELSE
                DO J=1,3
                    DO K=1,SURFACE_CURRENT2%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT2%SURFACE_FACES(J,IPZ))
                        I1 = SURFACE_CURRENT2%POINT_FACE_CONNECTION(K,SURFACE_CURRENT2%SURFACE_FACES(J,IPZ))
                        
                        CALL DISTANCE_FACE_FACE_TYPE(I,TYP1,I1,TYP2,DIR,  R,B)
                        
                        IF(B) THEN
                            SURFACE_CURRENT1%FACE_IMPACT_ZONE(TYP2+1, I) = I1
                            FLAG = .TRUE.
                            EXIT
                        END IF
                    END DO
                    
                    IF(FLAG) THEN
                        EXIT
                    END IF
                END DO
            END IF
            
            IF(.NOT. FLAG) THEN
                !WRITE(*,*) 'ERROR'
                !CALL DISTANCE_SURFACE_FACE_TYPE(I,TYP1,TYP2,DIR,  R,IDX)
                SURFACE_CURRENT1%FACE_IMPACT_ZONE(TYP2+1, I) = 0
            END IF
        END DO
        
    END SUBROUTINE UPDATE_IMPACT_ZONE
    
    SUBROUTINE POINT_FACE_CONNECTING(TYP)
        IMPLICIT NONE
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        
        INTEGER::TYP
        
        INTEGER :: I, I1,I2, J,J0,J1, K, TEMP
        INTEGER, ALLOCATABLE :: POINT_INDEX(:)
        
        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        END IF
        IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        END IF
        IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF
        
        
        SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM = 0
        
        
        DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
            ! Should modify for square case
            !IF(TYP==0) THEN
                DO J=1,3
                    SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT%SURFACE_FACES(J,I)) = SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT%SURFACE_FACES(J,I)) + 1
                    SURFACE_CURRENT%POINT_FACE_CONNECTION(SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT%SURFACE_FACES(J,I)),SURFACE_CURRENT%SURFACE_FACES(J,I)) = I
                END DO
            !ELSE
            !    DO J=1,4
            !        SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT%SURFACE_FACES(J,I)) = SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT%SURFACE_FACES(J,I)) + 1
            !        SURFACE_CURRENT%POINT_FACE_CONNECTION(SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(SURFACE_CURRENT%SURFACE_FACES(J,I)),SURFACE_CURRENT%SURFACE_FACES(J,I)) = I
            !    END DO
            !END IF
        END DO
        
        DO I=1,SURFACE_CURRENT%SURFACE_POINTS_NUM      
            ALLOCATE(POINT_INDEX(SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)))
            DO J=1,SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
                J0 = SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)
                DO K=1,3
                    IF(SURFACE_CURRENT%SURFACE_FACES(K,J0)==I) THEN
                        POINT_INDEX(J) = K
                        EXIT
                    END IF
                END DO
            END DO
            
            DO J=1,SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
                J0 = SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)
                I1 = SURFACE_CURRENT%SURFACE_FACES(MOD(POINT_INDEX(J)+1,3)+1, J0)
                DO K=J+1,SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
                    J1 = SURFACE_CURRENT%POINT_FACE_CONNECTION(K,I)
                    I2 = SURFACE_CURRENT%SURFACE_FACES(MOD(POINT_INDEX(K),3)+1, J1)
                    IF(I1==I2) THEN
                       TEMP = SURFACE_CURRENT%POINT_FACE_CONNECTION(J+1,I)
                       SURFACE_CURRENT%POINT_FACE_CONNECTION(J+1,I) = SURFACE_CURRENT%POINT_FACE_CONNECTION(K,I)
                       SURFACE_CURRENT%POINT_FACE_CONNECTION(K,I) = TEMP
                       
                       TEMP = POINT_INDEX(J+1)
                       POINT_INDEX(J+1) = POINT_INDEX(K)
                       POINT_INDEX(K) = TEMP
                       
                       EXIT
                    END IF
                END DO
            END DO
            DEALLOCATE(POINT_INDEX)
        END DO
        
    END SUBROUTINE POINT_FACE_CONNECTING
    
    SUBROUTINE FIND_POINT_TYPE(TYP, RETURN_RIDGE_EDGE)
	IMPLICIT NONE
        INTEGER :: TYP
        LOGICAL, OPTIONAL :: RETURN_RIDGE_EDGE(:,:)
        
        REAL(8), ALLOCATABLE :: FACE_AREA(:)
        
        INTEGER :: I,J,K,L, ITER, TEMP_I
        INTEGER :: I1,I2,I3, J1
        REAL(8) :: V1(3),V2(3),R
        REAL(8), ALLOCATABLE :: NORMAL(:,:)
        
        INTEGER :: CON_NUM
        
        REAL(8), ALLOCATABLE :: N(:,:), W(:)
        REAL(8) :: W_SUM
        
        REAL(8) :: E_VALUE(3)
        REAL(8) :: E_VECTOR(3,3), DUMMY(3,3)
        
        INTEGER :: JMIN
        REAL(8) :: TEMP_E_VALUE, TEMP_E_VECTOR(3)
        
        REAL(8) :: THETA_A, PHI_C, PHI_R, CHI_C, CHI_R
        LOGICAL :: B1, B2, B3

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
        
        ALLOCATE(FACE_AREA(SURFACE_CURRENT%SURFACE_FACES_NUM))
        ALLOCATE(NORMAL(3,SURFACE_CURRENT%SURFACE_FACES_NUM))
        
        DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
            
            I1 = SURFACE_CURRENT%SURFACE_FACES(1,I)
            I2 = SURFACE_CURRENT%SURFACE_FACES(2,I)
            I3 = SURFACE_CURRENT%SURFACE_FACES(3,I)
            
            V1 = SURFACE_CURRENT%SURFACE_POINTS(:,I2) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
            V2 = SURFACE_CURRENT%SURFACE_POINTS(:,I3) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
            
            !WRITE(*,*) SURFACE_CURRENT%SURFACE_POINTS(:,I1)
            !WRITE(*,*) SURFACE_CURRENT%SURFACE_POINTS(:,I2)
            !WRITE(*,*) SURFACE_CURRENT%SURFACE_POINTS(:,I3)
            
            CALL VEC_CURL1(V1,V2, NORMAL(:,I))
            
            R = SQRT(DOT_PRODUCT(NORMAL(:,I), NORMAL(:,I)))
if(R<minerror) then
write(*,*) I
write(*,*) i1,i2,i3
write(*,*) SURFACE_CURRENT%SURFACE_POINTS(:,I1)
write(*,*) SURFACE_CURRENT%SURFACE_POINTS(:,I2)
write(*,*) SURFACE_CURRENT%SURFACE_POINTS(:,I3)
end if
                       
            FACE_AREA(I) = R/2.
            
            NORMAL(:,I) = NORMAL(:,I) / R
            
        END DO
        
        DO I=1,SURFACE_CURRENT%SURFACE_POINTS_NUM         
            CON_NUM = SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
            
            ALLOCATE(N(CON_NUM,3))
            ALLOCATE(W(CON_NUM))
            
            W_SUM = 0.
            THETA_A = 0.
            DO J = 1,CON_NUM
                W_SUM = W_SUM + FACE_AREA(SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                
                I1 = SURFACE_CURRENT%SURFACE_FACES(1,SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                I2 = SURFACE_CURRENT%SURFACE_FACES(2,SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                I3 = SURFACE_CURRENT%SURFACE_FACES(3,SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                
                IF(I==I1) THEN
                ELSE IF(I==I2) THEN
                    TEMP_I = I1
                    I1 = I2
                    I2 = I3
                    I3 = TEMP_I
                ELSE
                    TEMP_I = I1
                    I1 = I3
                    I3 = I2
                    I2 = TEMP_I
                END IF
                
                V1 = SURFACE_CURRENT%SURFACE_POINTS(:,I2) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
                V2 = SURFACE_CURRENT%SURFACE_POINTS(:,I3) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
                THETA_A = THETA_A + ACOS(MAX(-1., MIN(1., DOT_PRODUCT(V1,V2)/SQRT(DOT_PRODUCT(V1,V1) * DOT_PRODUCT(V2,V2)) )) )
            END DO
            
            DO J = 1,CON_NUM
                N(J,:) = NORMAL(:,SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                W(J) = FACE_AREA(SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)) / W_SUM
            END DO
            
            DO K=1,3
                DO L=1,3
                    E_VECTOR(K,L) = 0.
                    DO J = 1,CON_NUM
                        E_VECTOR(K,L) = E_VECTOR(K,L) + W(J)*N(J,K)*N(J,L)
                    END DO
                END DO
            END DO
            
            ! COMPUTING EIGEN VALUE & EIGEN VECTOR OF LS_MATRIX
            
            CALL SVDCMP_ROUTINE(E_VECTOR,3,3,3,3,E_VALUE,DUMMY)
            
            DO J=1,3-1
                JMIN = J
                DO K=J+1,3
                    IF(E_VALUE(K)>E_VALUE(JMIN)) THEN
                        JMIN = K
                    END IF
                END DO
                
                IF(J .NE. JMIN) THEN
                    TEMP_E_VALUE = E_VALUE(J)
                    TEMP_E_VECTOR = E_VECTOR(:,J)
                    
                    E_VALUE(J) = E_VALUE(JMIN)
                    E_VECTOR(:,J) = E_VECTOR(:,JMIN)
                    
                    E_VALUE(JMIN) = TEMP_E_VALUE
                    E_VECTOR(:,JMIN) = TEMP_E_VECTOR(:)
                END IF
            END DO
            
            THETA_A = THETA_A - 2.*PI
        
            PHI_R = 14. * PI/180.
            PHI_C = 45. * PI/180.
           
            CHI_R = 2.*(TAN(PHI_R/2.))**2
            CHI_C = 2.*(TAN(PHI_C/2.))**2
       
            IF(E_VALUE(3)/E_VALUE(1) > CHI_C .OR. ABS(THETA_A) >= PI/2.) THEN
                SURFACE_CURRENT%POINT_TYPE(I) = 3
            ELSE IF(E_VALUE(2)/E_VALUE(1) > CHI_R) THEN
                SURFACE_CURRENT%POINT_TYPE(I) = 2
            ELSE
                SURFACE_CURRENT%POINT_TYPE(I) = 1
            END IF
   
            DEALLOCATE(N)
            DEALLOCATE(W)

            IF(SURFACE_CURRENT%POINT_TYPE(I)==3) THEN
		B1 = .FALSE.
		B2 = .FALSE.
		B3 = .FALSE.
		DO ITER = 1, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)	
		    J1 = SURFACE_CURRENT%POINT_FACE_CONNECTION(ITER,I)
		    IF(SURFACE_CURRENT%FACE_ONINTERFACE(J1)==1-TYP) THEN
			B1 = .TRUE.
		    ELSEIF(SURFACE_CURRENT%FACE_ONINTERFACE(J1)==2) THEN
			B2 = .TRUE.
		    ELSEIF(SURFACE_CURRENT%FACE_ONINTERFACE(J1)==-1) THEN
			B3 = .TRUE.
		    ELSE
		    END IF
		END DO

		IF(B1 .AND. B2) THEN
		    SURFACE_CURRENT%POINT_TYPE(I) = 5
		END IF
            ELSE
            
                B1 = .FALSE.
                B2 = .FALSE.
                DO ITER = 1, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)	
                    J1 = SURFACE_CURRENT%POINT_FACE_CONNECTION(ITER,I)
                    IF(SURFACE_CURRENT%FACE_ONINTERFACE(J1)==1-TYP) THEN
                        B1 = .TRUE.
                    ELSEIF(SURFACE_CURRENT%FACE_ONINTERFACE(J1)==2) THEN
                        B2 = .TRUE.
                    ELSE
                    END IF
                END DO
                
                IF(B1 .AND. B2) THEN
                    SURFACE_CURRENT%POINT_TYPE(I) = 4
                END IF

            END IF
            
        END DO
        
        DEALLOCATE(NORMAL)
        DEALLOCATE(FACE_AREA)
        
        IF(PRESENT(RETURN_RIDGE_EDGE)) THEN
            CALL FILTERING_POINT_TYPE(TYP, RETURN_RIDGE_EDGE)
        ELSE
            CALL FILTERING_POINT_TYPE(TYP)
        END IF
        
    END SUBROUTINE FIND_POINT_TYPE
    
    
    SUBROUTINE DIHEDRAL_ANGLE(POINT_NUM, POINT, FACE_NUM, FACE, I, I1,        ANGLE)
        IMPLICIT NONE
        INTEGER :: POINT_NUM
        REAL(8) :: POINT(3,POINT_NUM)
        INTEGER :: FACE_NUM
        INTEGER :: FACE(3,FACE_NUM)
        
        INTEGER :: I, I1
        REAL(8) :: V1(3), V2(3), V3(3), V4(3), N(3), N1(3), R, ANGLE
        
        V1 = POINT(:,FACE(2,I)) - POINT(:,FACE(1,I))
        V2 = POINT(:,FACE(3,I)) - POINT(:,FACE(1,I))
        
        CALL VEC_CURL1(V1,V2,N)
        N = N/SQRT(DOT_PRODUCT(N,N))
        
        V3 = POINT(:,FACE(2,I1)) - POINT(:,FACE(1,I1))
        V4 = POINT(:,FACE(3,I1)) - POINT(:,FACE(1,I1))
        
        CALL VEC_CURL1(V3,V4,N1)
        N1 = N1/SQRT(DOT_PRODUCT(N1,N1))
        
        R = DOT_PRODUCT(N,N1)
        ANGLE = ACOS(MAX(-1., MIN(1., R )) )
        
    END SUBROUTINE DIHEDRAL_ANGLE
    
    
    SUBROUTINE FACE_NEIGHBOR_FACE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, J0, DIR,      J1)
	IMPLICIT NONE
        INTEGER :: FACE_NUM
        INTEGER :: FACE(:,:)
        INTEGER :: CONNECTION_NUM(:)
        INTEGER :: CONNECTION(:,:)
        
        
        INTEGER :: J0, J1
        INTEGER :: DIR
        
        INTEGER :: I0, NUM, I
        
        I0 = FACE(DIR,J0)
        NUM = CONNECTION_NUM(I0)
        
        DO I=1,NUM
            IF(J0 == CONNECTION(I,I0)) THEN
                J1 = CONNECTION(MOD(I+NUM-2, NUM) + 1,I0)
                EXIT
            END IF
        END DO
        
    END SUBROUTINE FACE_NEIGHBOR_FACE
    
    SUBROUTINE POINT_NEIGHBOR_POINT(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, I0, DIR,      I1)
	IMPLICIT NONE
        INTEGER :: FACE_NUM
        INTEGER :: FACE(:,:)
        INTEGER :: CONNECTION_NUM(:)
        INTEGER :: CONNECTION(:,:)
        
        INTEGER :: I0, I1, I
        INTEGER :: DIR
        
        INTEGER :: J0
        
        J0 = CONNECTION(DIR,I0)
        
        DO I=1,3
            IF(I0 == FACE(I,J0)) THEN
                I1 = FACE(MOD(I, 3) + 1,J0)
                EXIT
            END IF
        END DO
        
    END SUBROUTINE POINT_NEIGHBOR_POINT
    
    
    SUBROUTINE FILTERING_POINT_TYPE(TYP, RETURN_RIDGE_EDGE)
	IMPLICIT NONE
        INTEGER :: TYP
        LOGICAL, OPTIONAL :: RETURN_RIDGE_EDGE(:,:)
        
        REAL(8), ALLOCATABLE :: FACE_AREA(:)
        
        INTEGER :: I,J,K,L, ITER, TEMP_I, T, JMIN, JMAX
        INTEGER :: I1,I2,I3, J1, J2, JJ
        REAL(8) :: V1(3),V2(3),R, V(3), RMIN, RMAX
        REAL(8), ALLOCATABLE :: NORMAL(:,:)
        
        INTEGER :: CON_NUM
        
        REAL(8), ALLOCATABLE :: N(:,:), W(:)
        REAL(8) :: W_SUM
        
        REAL(8) :: E_VALUE(3)
        REAL(8) :: DUMMY(3,3)
        REAL(8), ALLOCATABLE :: E_VECTOR(:,:,:)
        
        REAL(8) :: TEMP_E_VALUE, TEMP_E_VECTOR(3)
        
        REAL(8) :: THETA_A, PHI_C, PHI_R, CHI_C, CHI_R
        LOGICAL :: B1, B2, B3
        
        REAL(8) :: ANGLE
        LOGICAL, ALLOCATABLE :: RIDGE_EDGE(:,:)
        INTEGER :: RIDGE_EDGE_NUM
        INTEGER, ALLOCATABLE :: NEW_POINT_TYPE(:)
        
        LOGICAL :: B
        
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
        
        ALLOCATE(E_VECTOR(3,3,SURFACE_CURRENT%SURFACE_POINTS_NUM))
        
        ALLOCATE(FACE_AREA(SURFACE_CURRENT%SURFACE_FACES_NUM))
        ALLOCATE(NORMAL(3,SURFACE_CURRENT%SURFACE_FACES_NUM))
        
        DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
            
            I1 = SURFACE_CURRENT%SURFACE_FACES(1,I)
            I2 = SURFACE_CURRENT%SURFACE_FACES(2,I)
            I3 = SURFACE_CURRENT%SURFACE_FACES(3,I)
            
            V1 = SURFACE_CURRENT%SURFACE_POINTS(:,I2) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
            V2 = SURFACE_CURRENT%SURFACE_POINTS(:,I3) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
            
            CALL VEC_CURL1(V1,V2, NORMAL(:,I))
            
            R = SQRT(DOT_PRODUCT(NORMAL(:,I), NORMAL(:,I)))
                       
            FACE_AREA(I) = R/2.
            
            NORMAL(:,I) = NORMAL(:,I) / R
            
        END DO
        
        DO I=1,SURFACE_CURRENT%SURFACE_POINTS_NUM         
            CON_NUM = SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
            
            ALLOCATE(N(CON_NUM,3))
            ALLOCATE(W(CON_NUM))
            
            W_SUM = 0.
            THETA_A = 0.
            DO J = 1,CON_NUM
                W_SUM = W_SUM + FACE_AREA(SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                
                I1 = SURFACE_CURRENT%SURFACE_FACES(1,SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                I2 = SURFACE_CURRENT%SURFACE_FACES(2,SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                I3 = SURFACE_CURRENT%SURFACE_FACES(3,SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                
                IF(I==I1) THEN
                ELSE IF(I==I2) THEN
                    TEMP_I = I1
                    I1 = I2
                    I2 = I3
                    I3 = TEMP_I
                ELSE
                    TEMP_I = I1
                    I1 = I3
                    I3 = I2
                    I2 = TEMP_I
                END IF
                
                V1 = SURFACE_CURRENT%SURFACE_POINTS(:,I2) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
                V2 = SURFACE_CURRENT%SURFACE_POINTS(:,I3) - SURFACE_CURRENT%SURFACE_POINTS(:,I1)
                THETA_A = THETA_A + ACOS(MAX(-1., MIN(1., DOT_PRODUCT(V1,V2)/SQRT(DOT_PRODUCT(V1,V1) * DOT_PRODUCT(V2,V2)) )) )
            END DO
            
            DO J = 1,CON_NUM
                N(J,:) = NORMAL(:,SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I))
                W(J) = FACE_AREA(SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)) / W_SUM
            END DO
            
            DO K=1,3
                DO L=1,3
                    E_VECTOR(K,L,I) = 0.
                    DO J = 1,CON_NUM
                        E_VECTOR(K,L,I) = E_VECTOR(K,L,I) + W(J)*N(J,K)*N(J,L)
                    END DO
                END DO
            END DO
            
            ! COMPUTING EIGEN VALUE & EIGEN VECTOR OF LS_MATRIX
            
            CALL SVDCMP(E_VECTOR(:,:,I),3,3,3,3,E_VALUE,DUMMY)
            
            DO J=1,3-1
                JMIN = J
                DO K=J+1,3
                    IF(E_VALUE(K)>E_VALUE(JMIN)) THEN
                        JMIN = K
                    END IF
                END DO
                
                IF(J .NE. JMIN) THEN
                    TEMP_E_VALUE = E_VALUE(J)
                    TEMP_E_VECTOR = E_VECTOR(:,J,I)
                    
                    E_VALUE(J) = E_VALUE(JMIN)
                    E_VECTOR(:,J,I) = E_VECTOR(:,JMIN,I)
                    
                    E_VALUE(JMIN) = TEMP_E_VALUE
                    E_VECTOR(:,JMIN,I) = TEMP_E_VECTOR(:)
                END IF
            END DO
   
            DEALLOCATE(N)
            DEALLOCATE(W)
            
        END DO
        
        DEALLOCATE(NORMAL)
        DEALLOCATE(FACE_AREA)
        
        ALLOCATE(RIDGE_EDGE(3,SURFACE_CURRENT%SURFACE_FACES_NUM))
        ALLOCATE(NEW_POINT_TYPE(SURFACE_CURRENT%SURFACE_POINTS_NUM))
        DO I=1,SURFACE_CURRENT%SURFACE_POINTS_NUM
            NEW_POINT_TYPE(I) = SURFACE_CURRENT%POINT_TYPE(I)
        END DO
        
        RIDGE_EDGE(:,:) = .FALSE.
        
        DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
            DO K=1,3
                CALL FACE_NEIGHBOR_FACE(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION,I,K, I1)
                CALL DIHEDRAL_ANGLE(SURFACE_CURRENT%SURFACE_POINTS_NUM, SURFACE_CURRENT%SURFACE_POINTS, SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, I, I1,        ANGLE)
                
                J1 = SURFACE_CURRENT%SURFACE_FACES(K,I)
                J2 = SURFACE_CURRENT%SURFACE_FACES(MOD(K,3)+1,I)
                
                IF(ANGLE > 30. * PI/180.) THEN
                    RIDGE_EDGE(K,I) = .TRUE.
                ELSE IF(NEW_POINT_TYPE(J1)>=4 .AND. NEW_POINT_TYPE(J2)>=4) THEN
                    RIDGE_EDGE(K,I) = .TRUE.
                ELSE IF(ANGLE > 8. * PI/180.) THEN
                    
                    DO T=1,2
                        IF(T==1) THEN
                            J = J1
                        ELSE
                            J = J2
                        END IF
                        
                        IF(NEW_POINT_TYPE(J)>1) THEN
                            RMAX = 0.
                            JMAX = 0
                            RMIN = MAX(DOMAIN_MAX(1) - DOMAIN_MIN(1), DOMAIN_MAX(2) - DOMAIN_MIN(2), DOMAIN_MAX(3) - DOMAIN_MIN(3))
                            JMIN = 0
                            
                            DO L=1,SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(J)
                                CALL POINT_NEIGHBOR_POINT(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION, J, L, JJ)
                                V = SURFACE_CURRENT%SURFACE_POINTS(:,JJ) - SURFACE_CURRENT%SURFACE_POINTS(:,J)
                                R = DOT_PRODUCT(V, E_VECTOR(:,3,J))/SQRT(DOT_PRODUCT(V,V))
                                
                                IF(R>RMAX) THEN
                                    RMAX = R
                                    JMAX = JJ
                                END IF
                                IF(R<RMIN) THEN
                                    RMIN = R
                                    JMIN = JJ
                                END IF
                            END DO
                            
                            IF(JMAX==J1 .OR. JMIN==J1 .OR. JMAX==J2 .OR. JMIN==J2) THEN
                                RIDGE_EDGE(K,I) = .TRUE.
                                EXIT
                            END IF
                        END IF
                    END DO
                END IF
            END DO
        END DO
        
        B = .TRUE.
        DO WHILE(B)
            B = .FALSE.
            
            DO J=1,SURFACE_CURRENT%SURFACE_POINTS_NUM
                RIDGE_EDGE_NUM = 0
                
                DO L=1,SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(J)
                    I = SURFACE_CURRENT%POINT_FACE_CONNECTION(L,J)
                    DO K=1,3
                        IF(SURFACE_CURRENT%SURFACE_FACES(K,I)==J) THEN
                            EXIT
                        END IF
                    END DO
                    
                    IF(RIDGE_EDGE(K,I)) THEN
                        RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
                    END IF
                END DO
                
                IF(RIDGE_EDGE_NUM > 2) THEN
                    IF(NEW_POINT_TYPE(J)==4) THEN
                        B = .TRUE.
                        NEW_POINT_TYPE(J) = 5
                    ELSE IF(NEW_POINT_TYPE(J)==1 .OR. NEW_POINT_TYPE(J)==2) THEN
                        B = .TRUE.
                        NEW_POINT_TYPE(J) = 3
                    END IF
                ELSE IF(RIDGE_EDGE_NUM == 2) THEN
                    !IF(NEW_POINT_TYPE(J)==4 .OR. NEW_POINT_TYPE(J)==5) THEN
                    IF(NEW_POINT_TYPE(J)==1) THEN
                        B = .TRUE.
                        NEW_POINT_TYPE(J) = 2
                    END IF
                ELSE IF(RIDGE_EDGE_NUM < 2) THEN
                    !IF(NEW_POINT_TYPE(J)==4 .OR. NEW_POINT_TYPE(J)==5) THEN
                    !    WRITE(*,*) 'ERROR'
                    IF(NEW_POINT_TYPE(J)==2) THEN
                        B = .TRUE.
                        NEW_POINT_TYPE(J) = 1
                    END IF
                END IF
            END DO
            
            DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
                DO K=1,3
                    J1 = SURFACE_CURRENT%SURFACE_FACES(K,I)
                    J2 = SURFACE_CURRENT%SURFACE_FACES(MOD(K,3)+1,I)
                    
                    IF(RIDGE_EDGE(K,I) .AND. NEW_POINT_TYPE(J1)==1 .AND. NEW_POINT_TYPE(J2)==1) THEN
                        B = .TRUE.
                        
                        RIDGE_EDGE(K,I) = .FALSE.
                    END IF
                END DO
            END DO
        END DO
        
        
        B = .TRUE.
        DO WHILE(B)
            B = .FALSE.
            
            DO J=1,SURFACE_CURRENT%SURFACE_POINTS_NUM
                RIDGE_EDGE_NUM = 0
                
                DO L=1,SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(J)
                    I = SURFACE_CURRENT%POINT_FACE_CONNECTION(L,J)
                    DO K=1,3
                        IF(SURFACE_CURRENT%SURFACE_FACES(K,I)==J) THEN
                            EXIT
                        END IF
                    END DO
                    
                    IF(RIDGE_EDGE(K,I)) THEN
                        RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
                    END IF
                END DO
                
                IF(RIDGE_EDGE_NUM < 1) THEN
                    IF(NEW_POINT_TYPE(J)==2) THEN
                        B = .TRUE.
                        NEW_POINT_TYPE(J) = 1
                    END IF
                END IF
            END DO
            
            DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
                DO K=1,3
                    J1 = SURFACE_CURRENT%SURFACE_FACES(K,I)
                    J2 = SURFACE_CURRENT%SURFACE_FACES(MOD(K,3)+1,I)
                    
                    IF(RIDGE_EDGE(K,I) .AND. (NEW_POINT_TYPE(J1)==1 .OR. NEW_POINT_TYPE(J2)==1)) THEN
                        B = .TRUE.
                        
                        RIDGE_EDGE(K,I) = .FALSE.
                    END IF
                END DO
            END DO
        END DO
        
        DO J=1,SURFACE_CURRENT%SURFACE_POINTS_NUM
            SURFACE_CURRENT%POINT_TYPE(J) = NEW_POINT_TYPE(J)
        END DO
        
        IF(PRESENT(RETURN_RIDGE_EDGE)) THEN
            DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
                RETURN_RIDGE_EDGE(1,I) = RIDGE_EDGE(1,I)
                RETURN_RIDGE_EDGE(2,I) = RIDGE_EDGE(2,I)
                RETURN_RIDGE_EDGE(3,I) = RIDGE_EDGE(3,I)
            END DO
        END IF
        
        DEALLOCATE(NEW_POINT_TYPE)
        DEALLOCATE(RIDGE_EDGE)
        DEALLOCATE(E_VECTOR)
    END SUBROUTINE FILTERING_POINT_TYPE
    
    RECURSIVE SUBROUTINE AREA_DIVIDING_RECURSIVE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_REGION_ARRAY, DIVIDED_REGION_NUM, DIVIDED_BOUNDARY_ARRAY, USED_FACE, CURRENT_FACE)
	IMPLICIT NONE
        INTEGER :: FACE_NUM
        INTEGER :: FACE(:,:)
        INTEGER :: CONNECTION_NUM(:)
        INTEGER :: CONNECTION(:,:)
        INTEGER :: POINT_TYPE(:)
        
        INTEGER :: DIVIDED_REGION_ARRAY(:)
        INTEGER :: DIVIDED_REGION_NUM
        INTEGER :: DIVIDED_BOUNDARY_ARRAY(:,:)
        LOGICAL :: USED_FACE(:)
        INTEGER :: CURRENT_FACE, NEIGHBOR_FACE
        INTEGER :: I
        
        DIVIDED_REGION_ARRAY(CURRENT_FACE) = DIVIDED_REGION_NUM
        USED_FACE(CURRENT_FACE) = .TRUE.
        
        DO I=1,3
            IF(DIVIDED_BOUNDARY_ARRAY(I,CURRENT_FACE)==0) THEN
                CALL FACE_NEIGHBOR_FACE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, CURRENT_FACE, I, NEIGHBOR_FACE)
                
                IF(.NOT. USED_FACE(NEIGHBOR_FACE)) THEN
                    CALL AREA_DIVIDING_RECURSIVE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_REGION_ARRAY, DIVIDED_REGION_NUM, DIVIDED_BOUNDARY_ARRAY, USED_FACE, NEIGHBOR_FACE)
                END IF
            END IF
            
            
            !IF(POINT_TYPE(FACE(I, CURRENT_FACE))==1 .OR. POINT_TYPE(FACE(MOD(I,3) + 1, CURRENT_FACE))==1) THEN
            !    CALL FACE_NEIGHBOR_FACE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, CURRENT_FACE, I, NEIGHBOR_FACE)
            !    
            !    IF(.NOT. USED_FACE(NEIGHBOR_FACE)) THEN
            !        CALL AREA_DIVIDING_RECURSIVE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_REGION_ARRAY, DIVIDED_REGION_NUM, USED_FACE, NEIGHBOR_FACE)
            !    END IF
            !END IF
        END DO
        
    END SUBROUTINE AREA_DIVIDING_RECURSIVE
    
    SUBROUTINE AREA_DIVIDING(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_REGION_ARRAY, DIVIDED_REGION_NUM, DIVIDED_BOUNDARY_ARRAY)
	IMPLICIT NONE
        INTEGER :: FACE_NUM
        INTEGER :: FACE(:,:)
        INTEGER :: CONNECTION_NUM(:)
        INTEGER :: CONNECTION(:,:)
        INTEGER :: POINT_TYPE(:)
        
        INTEGER :: DIVIDED_REGION_ARRAY(FACE_NUM)
        INTEGER :: DIVIDED_REGION_NUM
        INTEGER :: DIVIDED_BOUNDARY_ARRAY(3,FACE_NUM)
        
        LOGICAL :: B
        LOGICAL, ALLOCATABLE :: USED_FACE(:)
        INTEGER :: CURRENT_FACE
        INTEGER :: I
        
        ALLOCATE(USED_FACE(FACE_NUM))
        USED_FACE(:) = .FALSE.
        
        DIVIDED_REGION_ARRAY(:) = 0
        DIVIDED_REGION_NUM = 0
        
            CURRENT_FACE = 1
            B = .TRUE.
            DO WHILE(B)
                B = .FALSE.
                DIVIDED_REGION_NUM = DIVIDED_REGION_NUM + 1
                CALL AREA_DIVIDING_RECURSIVE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_REGION_ARRAY, DIVIDED_REGION_NUM, DIVIDED_BOUNDARY_ARRAY, USED_FACE, CURRENT_FACE)
                
                DO I=1,FACE_NUM
                    IF(.NOT. USED_FACE(I)) THEN
                        B = .TRUE.
                        CURRENT_FACE = I
                        EXIT
                    END IF
                END DO
            END DO
        
        DEALLOCATE(USED_FACE)
        
    END SUBROUTINE AREA_DIVIDING
    
    SUBROUTINE AREA_DIVIDING_TYP(TYP)
	IMPLICIT NONE
        INTEGER :: TYP
        
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
        
        CALL AREA_DIVIDING(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, &
                     SURFACE_CURRENT%POINT_FACE_CONNECTION, SURFACE_CURRENT%POINT_TYPE, SURFACE_CURRENT%FACE_DIVIDED_REGION_ARRAY, SURFACE_CURRENT%FACE_DIVIDED_REGION_NUM, SURFACE_CURRENT%FACE_DIVIDED_BOUNDARY_ARRAY)
        
    END SUBROUTINE AREA_DIVIDING_TYP
    
    
    RECURSIVE SUBROUTINE BOUNDARY_DIVIDING_RECURSIVE(POINT_NUM, POINT, FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_BOUNDARY_ARRAY, DIVIDED_BOUNDARY_NUM, USED_EDGE, RIDGE_EDGE, CURRENT_EDGE)
	IMPLICIT NONE
        INTEGER :: POINT_NUM
        REAL(8) :: POINT(:,:)
        INTEGER :: FACE_NUM
        INTEGER :: FACE(:,:)
        INTEGER :: CONNECTION_NUM(:)
        INTEGER :: CONNECTION(:,:)
        INTEGER :: POINT_TYPE(:)
        
        INTEGER :: DIVIDED_BOUNDARY_ARRAY(:,:)
        INTEGER :: DIVIDED_BOUNDARY_NUM
        LOGICAL :: USED_EDGE(:,:)
        LOGICAL :: RIDGE_EDGE(:,:)
        INTEGER :: RIDGE_EDGE_NUM
        INTEGER :: CURRENT_EDGE(2), NEIGHBOR_EDGE(2)
        INTEGER :: I, J, K, I1, I2, L, J1, J2, T, JJ, S
        REAL(8) :: V1(3), V2(3)
        
        K = CURRENT_EDGE(1)
        I = CURRENT_EDGE(2)
        
        DIVIDED_BOUNDARY_ARRAY(K,I) = DIVIDED_BOUNDARY_NUM
        
        USED_EDGE(K,I) = .TRUE.
        
        CALL FACE_NEIGHBOR_FACE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, I, K, I1)
        
        DO L=1,3
            IF(FACE(L,I1)==FACE(MOD(K,3)+1,I)) THEN
                EXIT
            END IF
        END DO
        
        DIVIDED_BOUNDARY_ARRAY(L,I1) = DIVIDED_BOUNDARY_NUM
        
        USED_EDGE(L,I1) = .TRUE.
        
        J1 = FACE(K,I)
        J2 = FACE(MOD(K,3)+1,I)
        
        DO T=1,2
            IF(T==1) THEN
                J = J1
                JJ = J2
            ELSE
                J = J2
                JJ = J1
            END IF
            
            IF(POINT_TYPE(J)==3 .OR. POINT_TYPE(J)==5) THEN
                CYCLE
            END IF
            
            RIDGE_EDGE_NUM = 1
            
            DO L=1,CONNECTION_NUM(J)
                I2 = CONNECTION(L,J)
                DO S=1,3
                    IF(FACE(S,I2)==J) THEN
                        EXIT
                    END IF
                END DO
                
                IF(RIDGE_EDGE(S,I2) .AND. FACE(MOD(S,3)+1,I2).NE.JJ) THEN
                    RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
                    NEIGHBOR_EDGE(1) = S
                    NEIGHBOR_EDGE(2) = I2
                END IF
            END DO
            
            IF(RIDGE_EDGE_NUM > 1) THEN
            IF(.NOT. USED_EDGE(NEIGHBOR_EDGE(1),NEIGHBOR_EDGE(2)) .AND. RIDGE_EDGE_NUM == 2) THEN
                V1 = POINT(:,JJ) - POINT(:,J)
                V2 = POINT(:,J) - POINT(:,FACE(MOD(NEIGHBOR_EDGE(1),3)+1,NEIGHBOR_EDGE(2)))
                IF(ACOS(MAX(-1., MIN(1., DOT_PRODUCT(V1,V2)/SQRT(DOT_PRODUCT(V1,V1) * DOT_PRODUCT(V2,V2)) ))) < 30. * PI/180.) THEN
                    CALL BOUNDARY_DIVIDING_RECURSIVE(POINT_NUM, POINT, FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_BOUNDARY_ARRAY, DIVIDED_BOUNDARY_NUM, USED_EDGE, RIDGE_EDGE, NEIGHBOR_EDGE)
                END IF
            END IF
            END IF
        END DO
        
    END SUBROUTINE BOUNDARY_DIVIDING_RECURSIVE
    
    SUBROUTINE BOUNDARY_DIVIDING(POINT_NUM, POINT, FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_BOUNDARY_ARRAY, DIVIDED_BOUNDARY_NUM, RIDGE_EDGE)
	IMPLICIT NONE
        INTEGER :: POINT_NUM
        REAL(8) :: POINT(:,:)
        INTEGER :: FACE_NUM
        INTEGER :: FACE(:,:)
        INTEGER :: CONNECTION_NUM(:)
        INTEGER :: CONNECTION(:,:)
        INTEGER :: POINT_TYPE(:)
        
        INTEGER :: DIVIDED_BOUNDARY_ARRAY(3,FACE_NUM)
        INTEGER :: DIVIDED_BOUNDARY_NUM
        
        LOGICAL :: RIDGE_EDGE(3,FACE_NUM)
        
        LOGICAL :: B
        LOGICAL, ALLOCATABLE :: USED_EDGE(:,:)
        INTEGER :: CURRENT_EDGE(2)
        INTEGER :: I,K
        
        ALLOCATE(USED_EDGE(3,FACE_NUM))
        USED_EDGE(:,:) = .FALSE.
        
        DIVIDED_BOUNDARY_ARRAY(:,:) = 0
        DIVIDED_BOUNDARY_NUM = 0
        
            B = .TRUE.
            DO WHILE(B)
                B = .FALSE.
                
                DO I=1,FACE_NUM
                    DO K=1,3
                        IF(.NOT. USED_EDGE(K,I) .AND. RIDGE_EDGE(K,I)) THEN
                            B = .TRUE.
                            CURRENT_EDGE(1) = K
                            CURRENT_EDGE(2) = I
                            EXIT
                        END IF
                    END DO
                END DO
                
                IF(B) THEN
                    DIVIDED_BOUNDARY_NUM = DIVIDED_BOUNDARY_NUM + 1
                    CALL BOUNDARY_DIVIDING_RECURSIVE(POINT_NUM, POINT, FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, POINT_TYPE, DIVIDED_BOUNDARY_ARRAY, DIVIDED_BOUNDARY_NUM, USED_EDGE, RIDGE_EDGE, CURRENT_EDGE)
                END IF
            END DO
        
        DEALLOCATE(USED_EDGE)
        
    END SUBROUTINE BOUNDARY_DIVIDING
    
    SUBROUTINE BOUNDARY_DIVIDING_TYP(TYP)
	IMPLICIT NONE
        INTEGER :: TYP
        LOGICAL, ALLOCATABLE :: RIDGE_EDGE(:,:)
	
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
        
        ALLOCATE(RIDGE_EDGE(3,SURFACE_CURRENT%SURFACE_FACES_NUM))
        RIDGE_EDGE(:,:) = .FALSE.
        
        CALL FIND_POINT_TYPE(TYP, RIDGE_EDGE)
        
        CALL BOUNDARY_DIVIDING(SURFACE_CURRENT%SURFACE_POINTS_NUM, SURFACE_CURRENT%SURFACE_POINTS, SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, &
             SURFACE_CURRENT%POINT_FACE_CONNECTION, SURFACE_CURRENT%POINT_TYPE, SURFACE_CURRENT%FACE_DIVIDED_BOUNDARY_ARRAY, SURFACE_CURRENT%FACE_DIVIDED_BOUNDARY_NUM, RIDGE_EDGE)
        
        DEALLOCATE(RIDGE_EDGE)
	
    END SUBROUTINE BOUNDARY_DIVIDING_TYP
    
    
    
    SUBROUTINE FIND_PATCH_NUM(TYP)
	    IMPLICIT NONE
        INTEGER :: TYP
	    INTEGER :: PATCH_NUM
	    INTEGER :: I,J
	    LOGICAL :: B
	    REAL(8), ALLOCATABLE :: TEMP(:)
        
       
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
        
        PATCH_NUM = 0
	    ALLOCATE(TEMP(SURFACE_CURRENT%SURFACE_FACES_NUM)) 

        DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	        B = .TRUE.
	        IF(I .EQ. 1) THEN
	            PATCH_NUM = PATCH_NUM + 1
	            TEMP(PATCH_NUM) = SURFACE_CURRENT%FACE_LOCATION(I) 
	        ELSE 
	       	    DO J = 1, PATCH_NUM
	                IF(SURFACE_CURRENT%FACE_LOCATION(I) .EQ. TEMP(J)) THEN
		                B = .FALSE.
		                EXIT
		            END IF
		        END DO
		            IF(B) THEN
		                PATCH_NUM = PATCH_NUM + 1
		                TEMP(PATCH_NUM) = SURFACE_CURRENT%FACE_LOCATION(I) 
		                EXIT
	                END IF
            END IF
        END DO
        
	    SURFACE_CURRENT%SURFACE_PATCHES_NUM = PATCH_NUM

    END SUBROUTINE FIND_PATCH_NUM
    
    SUBROUTINE FIND_INTERFACE(TYP,CASE1)
        INTEGER::TYP, I1, I2, I3, K1, K2, K3, J
        REAL(8), ALLOCATABLE :: PHI(:)
        INTEGER :: I, FACE
	INTEGER :: IDX1, IDX2, IDX3, IDX4, IDX5, IDX6, IDX7, IDX8, IDX9, IDX10, IDX11, IDX12
        REAL(8) :: V1(3),V2(3),V3(3), R1, R2, R3, R, THRESH
	REAL(8) :: D1,D2,D3,D4,D5,D6,D7,D8,D9
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        TYPE(HASH), POINTER :: TEMP_HASH0, TEMP_HASH1, TEMP_HASH2, HASH0, HASH1, HASH2
        REAL(8) :: HASH_SIZE
	REAL(8) :: TEMP1, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6, AVER1, AVER2, MINTEMP
        INTEGER :: HASH_NUM1, HASH_NUM2, HASH_NUM3
        INTEGER :: N1
	CHARACTER(500) :: STR, STR1
        integer :: case1
	LOGICAL :: FLAG
	LOGICAL :: B
	
	HASH_SIZE = 0.03
if(case1==1) then
        
        CALL SETTING_HASH(HASH_SIZE, HASH_NUM1, HASH_NUM2, HASH_NUM3)
        
        NULLIFY(HASH0)
        CALL GENERATE_HASH(TYP,1,0,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH0)
        NULLIFY(HASH1)
        CALL GENERATE_HASH(TYP,1,1,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH1)
        NULLIFY(HASH2)
        CALL GENERATE_HASH(TYP,1,2,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH2)
       
        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        END IF
        IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        END IF
        IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF
        
        SURFACE_CURRENT%FACE_ONINTERFACE(:) = -1
        ALLOCATE(PHI(SURFACE_CURRENT%SURFACE_POINTS_NUM))
        
        TEMP_HASH0=>HASH0
        TEMP_HASH1=>HASH1
        TEMP_HASH2=>HASH2
        
SURFACE_CURRENT%POINT_DISTANCE = 100.
	IF (TYP/=TEMP_HASH0%TYP2) THEN
            DO WHILE(ASSOCIATED(TEMP_HASH0))
                    DO N1 = 1, TEMP_HASH0%ELEMENT_NUM1
                        I = TEMP_HASH0%ELEMENT1(N1)
		        DO J = 1, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
			    FACE = SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)
			    IF(SURFACE_CURRENT%FACE_ONINTERFACE(FACE) == -1) THEN
                                CALL DISTANCE_SURFACE_POINT_TYPE(I,TEMP_HASH0,PHI(I))
                                SURFACE_CURRENT%POINT_DISTANCE(1+TEMP_HASH0%TYP2,I) = PHI(I)
				EXIT
			    END IF 
			END DO
                    END DO
                TEMP_HASH0 => TEMP_HASH0%NEXT
            END DO
	END IF

	IF (TYP/=TEMP_HASH1%TYP2) THEN
            DO WHILE(ASSOCIATED(TEMP_HASH1))
                IF (TYP/=TEMP_HASH1%TYP2) THEN
                   DO N1 = 1, TEMP_HASH1%ELEMENT_NUM1
                        I = TEMP_HASH1%ELEMENT1(N1)
		        DO J = 1, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
			    FACE = SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)
			    IF(SURFACE_CURRENT%FACE_ONINTERFACE(FACE) == -1) THEN
                                CALL DISTANCE_SURFACE_POINT_TYPE(I,TEMP_HASH1,PHI(I))
                                SURFACE_CURRENT%POINT_DISTANCE(1+TEMP_HASH1%TYP2,I) = PHI(I)
				EXIT
			    END IF 
			END DO
                    END DO
                END IF
                TEMP_HASH1 => TEMP_HASH1%NEXT
            END DO
	END IF

	IF (TYP/=TEMP_HASH2%TYP2) THEN
            DO WHILE(ASSOCIATED(TEMP_HASH2))
                IF (TYP/=TEMP_HASH2%TYP2) THEN
                    DO N1 = 1, TEMP_HASH2%ELEMENT_NUM1
                        I = TEMP_HASH2%ELEMENT1(N1)
		        DO J = 1, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM(I)
			    FACE = SURFACE_CURRENT%POINT_FACE_CONNECTION(J,I)
			    IF(SURFACE_CURRENT%FACE_ONINTERFACE(FACE) == -1) THEN
                                CALL DISTANCE_SURFACE_POINT_TYPE(I,TEMP_HASH2,PHI(I))
                                SURFACE_CURRENT%POINT_DISTANCE(1+TEMP_HASH2%TYP2,I) = PHI(I)
				EXIT
			    END IF 
			END DO
                    END DO
                END IF
                TEMP_HASH2 => TEMP_HASH2%NEXT
            END DO
	END IF

            DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
  		IF(SURFACE_CURRENT%FACE_ONINTERFACE(I)==-1) THEN     
			V1 = SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(1,I))
			V2 = SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(2,I))
			V3 = SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(3,I))
		    
			R1 = SQRT(DOT_PRODUCT(V2-V1,V2-V1))
			R2 = SQRT(DOT_PRODUCT(V3-V2,V3-V2))
			R3 = SQRT(DOT_PRODUCT(V1-V3,V1-V3))
		    
			R = (R1+R2+R3)/3.

			I1 = SURFACE_CURRENT%SURFACE_FACES(1,I)
			I2 = SURFACE_CURRENT%SURFACE_FACES(2,I)
			I3 = SURFACE_CURRENT%SURFACE_FACES(3,I)

			IF(TYP==0) THEN
			    TEMP1 = SURFACE_CURRENT%POINT_DISTANCE(1+1,I1)
			    TEMP2 = SURFACE_CURRENT%POINT_DISTANCE(1+1,I2)
			    TEMP3 = SURFACE_CURRENT%POINT_DISTANCE(1+1,I3)
			    AVER1 = MAX(TEMP1,TEMP2,TEMP3)
			   !AVER1 = (ABS(TEMP1)+ABS(TEMP2)+ABS(TEMP3))/3.

			    TEMP4 = SURFACE_CURRENT%POINT_DISTANCE(1+2,I1)
			    TEMP5 = SURFACE_CURRENT%POINT_DISTANCE(1+2,I2)
			    TEMP6 = SURFACE_CURRENT%POINT_DISTANCE(1+2,I3)
			    !AVER2 = (ABS(TEMP4)+ABS(TEMP5)+ABS(TEMP6))/3.		
			    AVER2 = MAX(TEMP4,TEMP5,TEMP6)
			    IF(AVER2<R/5. .AND. AVER1 .GE. AVER2) THEN
				SURFACE_CURRENT%FACE_ONINTERFACE(I) = 2
			    ELSEIF(AVER1<R/5. .AND. AVER2 .GE. AVER1) THEN
				SURFACE_CURRENT%FACE_ONINTERFACE(I) = 1
			    END IF

			ELSE IF(TYP==1) THEN

			    TEMP1 = SURFACE_CURRENT%POINT_DISTANCE(1+0,I1)
			    TEMP2 = SURFACE_CURRENT%POINT_DISTANCE(1+0,I2)
			    TEMP3 = SURFACE_CURRENT%POINT_DISTANCE(1+0,I3)
			    AVER1 = MAX(TEMP1,TEMP2,TEMP3)

			    TEMP4 = SURFACE_CURRENT%POINT_DISTANCE(1+2,I1)
			    TEMP5 = SURFACE_CURRENT%POINT_DISTANCE(1+2,I2)
			    TEMP6 = SURFACE_CURRENT%POINT_DISTANCE(1+2,I3)
			    AVER2 = MAX(TEMP4,TEMP5,TEMP6)

			    IF(AVER2<R/5. .AND. AVER1 .GE. AVER2) THEN
				SURFACE_CURRENT%FACE_ONINTERFACE(I) = 2
			    ELSEIF(AVER1<R/5. .AND. AVER2 .GE. AVER1) THEN
				SURFACE_CURRENT%FACE_ONINTERFACE(I) = 0
			    END IF
			ELSE
			    TEMP1 = SURFACE_CURRENT%POINT_DISTANCE(1+0,I1)
			    TEMP2 = SURFACE_CURRENT%POINT_DISTANCE(1+0,I2)		    
			    TEMP3 = SURFACE_CURRENT%POINT_DISTANCE(1+0,I3)
			    AVER1 = MAX(TEMP1,TEMP2,TEMP3)

			    TEMP4 = SURFACE_CURRENT%POINT_DISTANCE(1+1,I1)
			    TEMP5 = SURFACE_CURRENT%POINT_DISTANCE(1+1,I2)
			    TEMP6 = SURFACE_CURRENT%POINT_DISTANCE(1+1,I3)
			    AVER2 = MAX(TEMP4,TEMP5,TEMP6)
			    MINTEMP = MIN(TEMP4, TEMP5, TEMP6)

			    IF(AVER2<R/5. .AND. AVER1 .GE. AVER2) THEN
				SURFACE_CURRENT%FACE_ONINTERFACE(I) = 1
			    ELSEIF(AVER1<R/5. .AND. AVER2 .GE. AVER1) THEN
				IF (MINTEMP>SURFACE_CURRENT%MESH_SIZE/10.) THEN
				    SURFACE_CURRENT%FACE_ONINTERFACE(I) = 0
				ELSE
				    SURFACE_CURRENT%FACE_ONINTERFACE(I) = 1
				END IF
			    END IF
			END IF
if(typ==1 .and. (i==202 .or. i==716)) then
write(*,*) i
write(*,*) aver1, aver2, R
write(*,*) 
end if
        	END IF
            END DO

        CALL DELETE_HASH(HASH0)
        CALL DELETE_HASH(HASH1)
        CALL DELETE_HASH(HASH2)
        CALL COLLAPSE_INTERFACE(TYP)
        NULLIFY(TEMP_HASH0)
        NULLIFY(TEMP_HASH1)
        NULLIFY(TEMP_HASH2)


        DEALLOCATE(PHI)

	WRITE(STR1,*) TYP
	STR = './output/surface/face_oninterface_3d_initial_' // TRIM(ADJUSTL(STR1)) // '.txt'
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    WRITE(21,*) SURFACE_CURRENT%FACE_ONINTERFACE(I)
	END DO
	!$OMP END DO
	CLOSE(21)
else if(case1==2) then 

        CALL SETTING_HASH(HASH_SIZE, HASH_NUM1, HASH_NUM2, HASH_NUM3)
       
        NULLIFY(HASH0)
        CALL GENERATE_HASH(TYP,2,0,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH0)
        NULLIFY(HASH1)
        CALL GENERATE_HASH(TYP,2,1,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH1)
        NULLIFY(HASH2)
        CALL GENERATE_HASH(TYP,2,2,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH2)
       
        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        END IF
        IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        END IF
        IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF
        
        SURFACE_CURRENT%FACE_ONINTERFACE = -1
        TEMP_HASH0=>HASH0
        TEMP_HASH1=>HASH1
        TEMP_HASH2=>HASH2
        DO WHILE(ASSOCIATED(TEMP_HASH0))
            DO N1 = 1, TEMP_HASH0%ELEMENT_NUM1
                I = TEMP_HASH0%ELEMENT1(N1)
		V1 = SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(1,I))
		V2 = SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(2,I))
		V3 = SURFACE_CURRENT%SURFACE_POINTS(:,SURFACE_CURRENT%SURFACE_FACES(3,I))
        
        	THRESH = (SQRT(DOT_PRODUCT(V2-V1,V2-V1)) + SQRT(DOT_PRODUCT(V3-V2,V3-V2)) + SQRT(DOT_PRODUCT(V3-V1,V3-V1)))/3.

                R1 = 1000.
                R2 = 1000.
                R3 = 1000.
		IDX1 = 0
		IDX2 = 0
		IDX3 = 0
                
                IF(TYP.NE.0) CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH0,  1, R1, IDX1)
                IF(TYP.NE.1) CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH1,  1, R2, IDX2)
                IF(TYP.NE.2) CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH2,  1, R3, IDX3)

                IF(ABS(R1) <= ABS(R2) .AND. ABS(R1) <= ABS(R3) .AND. ABS(R1) < THRESH/2.75 ) THEN
                    SURFACE_CURRENT%FACE_ONINTERFACE(I) = 0
                ELSE IF(ABS(R2) <= ABS(R1) .AND. ABS(R2) <= ABS(R3) .AND. ABS(R2) < THRESH/2.75) THEN
                    SURFACE_CURRENT%FACE_ONINTERFACE(I) = 1
                ELSE IF(ABS(R3) <= ABS(R1) .AND. ABS(R3) <= ABS(R2) .AND. ABS(R3) < THRESH/2.75) THEN
                    SURFACE_CURRENT%FACE_ONINTERFACE(I) = 2
                END IF

!		IF(SURFACE_CURRENT%FACE_ONINTERFACE(I)==-1) THEN
!		    IDX4 = 0
!		    IDX5 = 0
!		    IDX6 = 0
!		    IDX7 = 0
!		    IDX8 = 0
!		    IDX9 = 0
!		    IDX10 = 0
!		    IDX11 = 0
!		    IDX12 = 0
!		    D1 = 100.	
!		    D2 = 100.
!		    D3 = 100.
!		    D4 = 100.
!		    D5 = 100.
!		    D6 = 100.
!		    D7 = 100.
!		    D8 = 100.
!		    D9 = 100.
!
!		    IF(TYP .NE. 0) THEN
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH0, 1, D1, IDX4, NORMAL_FROM_POINT = .TRUE., ITER = 1)
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH0, 1, D2, IDX5, NORMAL_FROM_POINT = .TRUE., ITER = 2)
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH0, 1, D3, IDX6, NORMAL_FROM_POINT = .TRUE., ITER = 3)
!		    END IF		
!		    IF(TYP .NE. 1) THEN
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH1, 1, D4, IDX7, NORMAL_FROM_POINT = .TRUE., ITER = 1)
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH1, 1, D5, IDX8, NORMAL_FROM_POINT = .TRUE., ITER = 2)
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH1, 1, D6, IDX9, NORMAL_FROM_POINT = .TRUE., ITER = 3)
!		    END IF
!		    IF(TYP .NE. 2) THEN
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH2, 1, D7, IDX10, NORMAL_FROM_POINT = .TRUE., ITER = 1)
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH2, 1, D8, IDX11, NORMAL_FROM_POINT = .TRUE., ITER = 2)
!		        CALL DISTANCE_SURFACE_FACE_TYPE(N1, TEMP_HASH2, 1, D9, IDX12, NORMAL_FROM_POINT = .TRUE., ITER = 3)
!		    END IF

!                    IF((IDX1 .NE. IDX4 .OR. IDX1 .NE. IDX5 .OR. IDX1 .NE. IDX6) .AND. MIN(ABS(D1),ABS(D2),ABS(D3))<= ABS(R2) .AND. (ABS(D1)+ABS(D2)+ABS(D3))/3.<= ABS(R3) .AND. (ABS(D1)+ABS(D2)+ABS(D3))/3.<THRESH/3.) THEN
!                        SURFACE_CURRENT%FACE_ONINTERFACE(I) = 0
!                    ELSE IF((IDX2 .NE. IDX7 .OR. IDX2 .NE. IDX8 .OR. IDX2 .NE. IDX9) .AND. MIN(ABS(D4),ABS(D5),ABS(D6)).<= ABS(R1) .AND. (ABS(D4)+ABS(D5)+ABS(D6))/3.<= ABS(R3) .AND. (ABS(D4)+ABS(D5)+ABS(D6))/3.<THRESH/3.) THEN
!                        SURFACE_CURRENT%FACE_ONINTERFACE(I) = 1
!                    ELSE IF((IDX3 .NE. IDX10 .OR. IDX3 .NE. IDX11 .OR. IDX3 .NE. IDX12) .AND. MIN(ABS(D7),ABS(D8),ABS(D9)) .AND. (ABS(D7)+ABS(D8)+ABS(D9))/3.<= ABS(R2) .AND. (ABS(D7)+ABS(D8)+ABS(D9))/3.<THRESH/3.) THEN
!                        SURFACE_CURRENT%FACE_ONINTERFACE(I) = 2
!                    END IF
!if(typ==0 .and. (i==3066 .or. i==3100 .or. i==3144 .or. i==3157 .or. i==3158)) then
!if(typ==0 .and. i==6799) then
!write(*,*) i
!write(*,*) 'to_propel : '
!write(*,*) idx2, r2
!write(*,*) 'to case : '
!write(*,*) idx3, r3
!write(*,*) thresh
!write(*,*) 
!end if
!		END IF
            END DO
	    TEMP_HASH0 => TEMP_HASH0%NEXT
	    TEMP_HASH1 => TEMP_HASH1%NEXT
	    TEMP_HASH2 => TEMP_HASH2%NEXT
        END DO
        CALL DELETE_HASH(HASH0)
        CALL DELETE_HASH(HASH1)
        CALL DELETE_HASH(HASH2)
        
        IF(TYP==0) THEN
        DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
            CALL FACE_NEIGHBOR_FACE(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION,I,1, I1)
            CALL FACE_NEIGHBOR_FACE(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION,I,2, I2)
            CALL FACE_NEIGHBOR_FACE(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION,I,3, I3)
            
            K1 = SURFACE_CURRENT%FACE_ONINTERFACE(I1)
            K2 = SURFACE_CURRENT%FACE_ONINTERFACE(I2)
            K3 = SURFACE_CURRENT%FACE_ONINTERFACE(I3)
            
            IF(K1==K2 .AND. K1==1) THEN
                SURFACE_CURRENT%FACE_ONINTERFACE(I) = K1
            ELSE IF(K2==K3 .AND. K2==1) THEN
                SURFACE_CURRENT%FACE_ONINTERFACE(I) = K2
            ELSE IF(K3==K1 .AND. K3==1) THEN
                SURFACE_CURRENT%FACE_ONINTERFACE(I) = K3
            END IF
            
        END DO
        END IF
        
!        B = .TRUE.
!        DO WHILE(B)
!        B = .FALSE.
!        DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
!            IF(SURFACE_CURRENT%FACE_ONINTERFACE(I)==-1) THEN
!                CALL FACE_NEIGHBOR_FACE(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION,I,1, I1)
!                CALL FACE_NEIGHBOR_FACE(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION,I,2, I2)
!                CALL FACE_NEIGHBOR_FACE(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION,I,3, I3)
!                
!                K1 = SURFACE_CURRENT%FACE_ONINTERFACE(I1)
!                K2 = SURFACE_CURRENT%FACE_ONINTERFACE(I2)
!                K3 = SURFACE_CURRENT%FACE_ONINTERFACE(I3)
!                
!                IF(K1==K2 .AND. K1.NE.-1) THEN
!                    SURFACE_CURRENT%FACE_ONINTERFACE(I) = K1
!                    B = .TRUE.
!                ELSE IF(K2==K3 .AND. K2.NE.-1) THEN
!                    SURFACE_CURRENT%FACE_ONINTERFACE(I) = K2
!                    B = .TRUE.
!                ELSE IF(K3==K1 .AND. K3.NE.-1) THEN
!                    SURFACE_CURRENT%FACE_ONINTERFACE(I) = K3
!                    B = .TRUE.
!                END IF
!            END IF
!        END DO
!        END DO
        
        IF(TYP==2) THEN
	    FLAG = .FALSE.
	    CALL UPDATE_CASE_INTERFACE(FLAG)
        END IF
       
        NULLIFY(TEMP_HASH0)
        NULLIFY(TEMP_HASH1)
        NULLIFY(TEMP_HASH2)
	NULLIFY(SURFACE_CURRENT)
        
        CALL COLLAPSE_INTERFACE(TYP)

else if(case1==3) then

        IF (TYP==0) THEN
            SURFACE_CURRENT => SURFACE_FLUID
        END IF
        IF (TYP==1) THEN
            SURFACE_CURRENT => SURFACE_PROPEL
        END IF
        IF (TYP==2) THEN
            SURFACE_CURRENT => SURFACE_CASE
        END IF

	WRITE(STR1,*) TYP
	STR = './output/surface/face_oninterface_3d_initial_' // TRIM(ADJUSTL(STR1)) // '.txt'
	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT%SURFACE_FACES_NUM
	    READ(21,*) SURFACE_CURRENT%FACE_ONINTERFACE(I)
	END DO
	!$OMP END DO
	CLOSE(21)
end if
        
        NULLIFY(SURFACE_CURRENT)
    END SUBROUTINE FIND_INTERFACE
    
    
    RECURSIVE SUBROUTINE COLLAPSE_INTERFACE_RECURSIVE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, ONINTERFACE, FACE_CLUSTER, FACE_CLUSTER_NUM, USED_FACE, CURRENT_FACE, NEIGHBOR_ONINTERFACE)
	IMPLICIT NONE
        INTEGER :: FACE_NUM
        INTEGER :: FACE(:,:)
        INTEGER :: CONNECTION_NUM(:)
        INTEGER :: CONNECTION(:,:)
        INTEGER :: ONINTERFACE(:)
        
        INTEGER :: FACE_CLUSTER(:)
        INTEGER :: FACE_CLUSTER_NUM
        LOGICAL :: USED_FACE(:)
        INTEGER :: CURRENT_FACE, NEIGHBOR_FACE, NEIGHBOR_ONINTERFACE(:)
        INTEGER :: I
        
        FACE_CLUSTER_NUM = FACE_CLUSTER_NUM + 1
        FACE_CLUSTER(FACE_CLUSTER_NUM) = CURRENT_FACE
        USED_FACE(CURRENT_FACE) = .TRUE.
        
        DO I=1,3
            CALL FACE_NEIGHBOR_FACE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, CURRENT_FACE, I, NEIGHBOR_FACE)
            
            IF(ONINTERFACE(CURRENT_FACE)==ONINTERFACE(NEIGHBOR_FACE) .AND. .NOT. USED_FACE(NEIGHBOR_FACE)) THEN
                CALL COLLAPSE_INTERFACE_RECURSIVE(FACE_NUM, FACE, CONNECTION_NUM, CONNECTION, ONINTERFACE, FACE_CLUSTER, FACE_CLUSTER_NUM, USED_FACE, NEIGHBOR_FACE, NEIGHBOR_ONINTERFACE)
            ELSE IF(ONINTERFACE(CURRENT_FACE).NE.ONINTERFACE(NEIGHBOR_FACE)) THEN
                NEIGHBOR_ONINTERFACE(ONINTERFACE(NEIGHBOR_FACE)+2) = NEIGHBOR_ONINTERFACE(ONINTERFACE(NEIGHBOR_FACE)+2) + 1
            END IF
        END DO
        
    END SUBROUTINE COLLAPSE_INTERFACE_RECURSIVE
    
    SUBROUTINE COLLAPSE_INTERFACE(TYP)
	IMPLICIT NONE
        INTEGER :: TYP
        
        INTEGER, ALLOCATABLE :: FACE_CLUSTER(:)
        INTEGER :: FACE_CLUSTER_NUM
        LOGICAL, ALLOCATABLE :: USED_FACE(:)
        
        INTEGER :: CURRENT_FACE, NEIGHBOR_ONINTERFACE(4), RESULT_ONINTERFACE
        LOGICAL :: B
        INTEGER :: I,J

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
        
        ALLOCATE(FACE_CLUSTER(SURFACE_CURRENT%SURFACE_FACES_NUM))
        
        ALLOCATE(USED_FACE(SURFACE_CURRENT%SURFACE_FACES_NUM))
        USED_FACE(:) = .FALSE.
        
        CURRENT_FACE = 1
        B = .TRUE.
        DO WHILE(B)
            B = .FALSE.
            
            FACE_CLUSTER(:) = 0
            FACE_CLUSTER_NUM = 0
            
            NEIGHBOR_ONINTERFACE(:) = 0
            
            CALL COLLAPSE_INTERFACE_RECURSIVE(SURFACE_CURRENT%SURFACE_FACES_NUM, SURFACE_CURRENT%SURFACE_FACES, SURFACE_CURRENT%POINT_FACE_CONNECTION_NUM, SURFACE_CURRENT%POINT_FACE_CONNECTION, SURFACE_CURRENT%FACE_ONINTERFACE, FACE_CLUSTER, FACE_CLUSTER_NUM, USED_FACE, CURRENT_FACE, NEIGHBOR_ONINTERFACE)
            
            IF(FACE_CLUSTER_NUM < 20) THEN
                DO J=1,4
                    IF(NEIGHBOR_ONINTERFACE(J)==MAX(NEIGHBOR_ONINTERFACE(1),NEIGHBOR_ONINTERFACE(2),NEIGHBOR_ONINTERFACE(3),NEIGHBOR_ONINTERFACE(4))) THEN
                        RESULT_ONINTERFACE = J-2
                        EXIT
                    END IF
                END DO
                
                DO I=1,FACE_CLUSTER_NUM
                    SURFACE_CURRENT%FACE_ONINTERFACE(FACE_CLUSTER(I)) = RESULT_ONINTERFACE
                END DO
            END IF
            
            DO I=1,SURFACE_CURRENT%SURFACE_FACES_NUM
                IF(.NOT. USED_FACE(I)) THEN
                    B = .TRUE.
                    CURRENT_FACE = I
                    EXIT
                END IF
            END DO
        END DO
        
        DEALLOCATE(USED_FACE)
        
        DEALLOCATE(FACE_CLUSTER)
        
    END SUBROUTINE COLLAPSE_INTERFACE
    


    SUBROUTINE UPDATE_CASE_INTERFACE(FLAG)
        IMPLICIT NONE
        LOGICAL :: FLAG
        REAL(8) :: PHI1, PHI2, PHI3, PHI4, PHI5, PHI6, MINPHI
        INTEGER :: I, J, K, I1, J1, N1
        INTEGER, ALLOCATABLE :: TEMP_ONINTERFACE(:)
        LOGICAL :: B
        TYPE(HASH), POINTER :: TEMP_HASH0, TEMP_HASH1,  HASH0, HASH1
        REAL(8) :: HASH_SIZE
        INTEGER :: HASH_NUM1, HASH_NUM2, HASH_NUM3

IF (FLAG) THEN        
        FLAG = .FALSE.
        
        ALLOCATE(TEMP_ONINTERFACE(SURFACE_CASE%SURFACE_FACES_NUM))
        
        TEMP_ONINTERFACE(:) = SURFACE_CASE%FACE_ONINTERFACE(:)
        
        HASH_SIZE = 0.03
        CALL SETTING_HASH(HASH_SIZE, HASH_NUM1, HASH_NUM2, HASH_NUM3)
        NULLIFY(HASH0)
        CALL GENERATE_HASH(2,2,0,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH0)
        NULLIFY(HASH1)
        CALL GENERATE_HASH(2,2,1,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH1)
                       
        TEMP_HASH0 => HASH0
        TEMP_HASH1 => HASH1
        DO WHILE (ASSOCIATED(TEMP_HASH1))
            DO N1 = 1, TEMP_HASH0%ELEMENT_NUM1
                I = TEMP_HASH0%ELEMENT1(N1)
                IF (SURFACE_CASE%FACE_ONINTERFACE(I)==1) THEN
                    B = .FALSE.
                    DO J=1,3
                        J1 = SURFACE_CASE%SURFACE_FACES(J,I)
                        DO K=1,SURFACE_CASE%POINT_FACE_CONNECTION_NUM(J1)
                            I1 = SURFACE_CASE%POINT_FACE_CONNECTION(K,J1)
                            IF(SURFACE_CASE%FACE_ONINTERFACE(I1)==0) THEN
                                B = .TRUE.
                                EXIT
                            END IF
                        END DO
                    
                        IF(B) THEN
                            EXIT
                        END IF
                    END DO
                    
                    
                    IF(B) THEN
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(1,I),TEMP_HASH0,PHI1)
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(1,I),TEMP_HASH1,PHI2)
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(2,I),TEMP_HASH0,PHI3)
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(2,I),TEMP_HASH1,PHI4)
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(3,I),TEMP_HASH0,PHI5)
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(3,I),TEMP_HASH1,PHI6)
                        MINPHI = MIN(ABS(PHI2), ABS(PHI4), ABS(PHI6))
                        IF(ABS(PHI1)<ABS(PHI2) .AND. ABS(PHI3)<ABS(PHI4) .AND. ABS(PHI5)<ABS(PHI6) .AND. MINPHI>SURFACE_CASE%MESH_SIZE/2.) THEN
                            TEMP_ONINTERFACE(I) = 0
                        ELSE
                            TEMP_ONINTERFACE(I) = 1
                        END IF
                    END IF
                
                END IF
                
            END DO
            
            TEMP_HASH0=>TEMP_HASH0%NEXT
            TEMP_HASH1=>TEMP_HASH1%NEXT
        END DO
	
	NULLIFY(TEMP_HASH0)
	NULLIFY(TEMP_HASH1)
	CALL DELETE_HASH(HASH0)
	CALL DELETE_HASH(HASH1)
        
        DO I = 1, SURFACE_CASE%SURFACE_FACES_NUM
            IF(SURFACE_CASE%FACE_ONINTERFACE(I) .NE. TEMP_ONINTERFACE(I)) THEN
                FLAG = .TRUE.
            END IF
            
            SURFACE_CASE%FACE_ONINTERFACE(I) = TEMP_ONINTERFACE(I)
        END DO
        
        DEALLOCATE(TEMP_ONINTERFACE)

ELSE
        HASH_SIZE = 0.03
        CALL SETTING_HASH(HASH_SIZE, HASH_NUM1, HASH_NUM2, HASH_NUM3)
        NULLIFY(HASH1)
        CALL GENERATE_HASH(2,2,1,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH1)
                       
        TEMP_HASH1 => HASH1
        DO WHILE (ASSOCIATED(TEMP_HASH1))
            DO N1 = 1, TEMP_HASH1%ELEMENT_NUM1
                I = TEMP_HASH1%ELEMENT1(N1)
                IF (SURFACE_CASE%FACE_ONINTERFACE(I)==0) THEN
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(1,I),TEMP_HASH1,PHI2)
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(2,I),TEMP_HASH1,PHI4)
                        CALL DISTANCE_SURFACE_POINT_TYPE(SURFACE_CASE%SURFACE_FACES(3,I),TEMP_HASH1,PHI6)
                        MINPHI = MIN(ABS(PHI2), ABS(PHI4), ABS(PHI6))

                        IF(MINPHI<SURFACE_CASE%MESH_SIZE/2.) THEN
                            SURFACE_CASE%FACE_ONINTERFACE(I) = 1
                        END IF
                END IF
                
            END DO
            
            TEMP_HASH1=>TEMP_HASH1%NEXT
        END DO
	
	NULLIFY(TEMP_HASH1)
	CALL DELETE_HASH(HASH1)
END IF
    END SUBROUTINE UPDATE_CASE_INTERFACE


    SUBROUTINE FIND_RELATEDPT_ITER(I, CURRENT_HASH, POINT_USED, RMIN, JMIN)
        IMPLICIT NONE
        TYPE(HASH), POINTER :: CURRENT_HASH
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1, SURFACE_CURRENT2
        
        INTEGER :: I,J,N2
        REAL(8) :: R
        INTEGER :: JMIN
        REAL(8) :: RMIN
        LOGICAL, DIMENSION(:) :: POINT_USED
        
        IF (CURRENT_HASH%TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (CURRENT_HASH%TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF

        DO N2 = 1, CURRENT_HASH%ELEMENT_NUM2
	    J = CURRENT_HASH%ELEMENT2(N2)
	    !IF(.NOT. POINT_USED(J)) THEN
	        R = SQRT(DOT_PRODUCT(SURFACE_CURRENT2%SURFACE_POINTS(:,J)-SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,J)-SURFACE_CURRENT1%SURFACE_POINTS(:,I)))
	        IF(R<RMIN) THEN
	            RMIN = R
	            JMIN = J
	        END IF
	   ! END IF
	END DO
    END SUBROUTINE
    

    SUBROUTINE FIND_RELATEDPT(TYP1, TYP2, RMIN_MAX)
        IMPLICIT NONE
        TYPE(HASH), POINTER :: HASH0, TEMP_HASH
        TYPE(HASH), POINTER :: NBHD_HASH
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1, SURFACE_CURRENT2
	
        INTEGER :: TYP1, TYP2
        INTEGER :: I,J,K, I1, N1
        INTEGER :: JMIN
        REAL(8) :: RMIN, RMIN_MAX
        LOGICAL, ALLOCATABLE :: POINT_USED(:)
        LOGICAL :: B
	REAL(8) :: HASH_SIZE
	INTEGER :: HASH_NUM1, HASH_NUM2, HASH_NUM3
	
	CHARACTER(500) :: STR, STR1, STR2
	INTEGER :: CASE1
        
        IF (TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF

	CASE1 = 1
if(case1==1) THEN
        K = TYP2 + 1
        SURFACE_CURRENT1%POINT_RELATEDPT(K,:) = 0
        
        ALLOCATE(POINT_USED(SURFACE_CURRENT2%SURFACE_POINTS_NUM))
        
        POINT_USED = .FALSE.
        
        HASH_SIZE = 0.03

	CALL SETTING_HASH(HASH_SIZE, HASH_NUM1, HASH_NUM2, HASH_NUM3)

	NULLIFY(HASH0)
	CALL GENERATE_HASH(TYP1,1 ,TYP2,1,  1,1,1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, HASH0)
          
	NULLIFY(TEMP_HASH) 
	NULLIFY(NBHD_HASH)
        
        TEMP_HASH => HASH0
	DO WHILE(ASSOCIATED(TEMP_HASH))

            DO N1 = 1, TEMP_HASH%ELEMENT_NUM1
                I = TEMP_HASH%ELEMENT1(N1)
                B = .FALSE.
                DO J=1,SURFACE_CURRENT1%POINT_FACE_CONNECTION_NUM(I)
                    I1 = SURFACE_CURRENT1%POINT_FACE_CONNECTION(J,I)
                    IF(SURFACE_CURRENT1%FACE_ONINTERFACE(I1)==HASH0%TYP2) THEN
                        B = .TRUE.
			EXIT
                    END IF
                END DO

        	IF(B) THEN
	            JMIN = 0
	            RMIN = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1), DOMAIN_MAX(2)-DOMAIN_MIN(2), DOMAIN_MAX(3)-DOMAIN_MIN(3))
	            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	            NBHD_HASH => TEMP_HASH
		    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	    
	            NBHD_HASH => TEMP_HASH%LEFT
	            IF (ASSOCIATED(NBHD_HASH)) THEN
	                CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	            END IF
	    
	            NBHD_HASH => TEMP_HASH%RIGHT
	            IF (ASSOCIATED(NBHD_HASH)) THEN
	                CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	            END IF
	    
	            NBHD_HASH => TEMP_HASH%BOTTOM
	            IF (ASSOCIATED(NBHD_HASH)) THEN
	                CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	            END IF
	    
	            NBHD_HASH => TEMP_HASH%TOP
	            IF (ASSOCIATED(NBHD_HASH)) THEN
	                CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	            END IF
	    
	            NBHD_HASH => TEMP_HASH%BACK
	            IF (ASSOCIATED(NBHD_HASH)) THEN
	                CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	            END IF
	    
	            NBHD_HASH => TEMP_HASH%FRONT
	            IF (ASSOCIATED(NBHD_HASH)) THEN
	                CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	            END IF
	            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
	            IF (ASSOCIATED(TEMP_HASH%LEFT)) THEN                   
	                NBHD_HASH => TEMP_HASH%LEFT%BACK
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF

	                IF (ASSOCIATED(TEMP_HASH%LEFT%BACK)) THEN                    
	                    NBHD_HASH => TEMP_HASH%LEFT%BACK%TOP
	                    IF (ASSOCIATED(NBHD_HASH)) THEN
	                        CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                    END IF
	    
	                    NBHD_HASH => TEMP_HASH%LEFT%BACK%BOTTOM
	                    IF (ASSOCIATED(NBHD_HASH)) THEN
	                        CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                    END IF
	                END IF
	    
	                NBHD_HASH => TEMP_HASH%LEFT%TOP
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                NBHD_HASH => TEMP_HASH%LEFT%BOTTOM
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                NBHD_HASH => TEMP_HASH%LEFT%FRONT
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                IF (ASSOCIATED(TEMP_HASH%LEFT%FRONT)) THEN
	                    NBHD_HASH => TEMP_HASH%LEFT%FRONT%TOP
	                    IF (ASSOCIATED(NBHD_HASH)) THEN
	                        CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                    END IF
	    
	                    NBHD_HASH => TEMP_HASH%LEFT%FRONT%BOTTOM
	                    IF (ASSOCIATED(NBHD_HASH)) THEN
	                        CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                    END IF
	                END IF
	            END IF
	            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	            IF (ASSOCIATED(TEMP_HASH%RIGHT)) THEN
	                NBHD_HASH => TEMP_HASH%RIGHT%BACK
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                IF (ASSOCIATED(TEMP_HASH%RIGHT%BACK)) THEN
	                    NBHD_HASH => TEMP_HASH%RIGHT%BACK%TOP
	                    IF (ASSOCIATED(NBHD_HASH)) THEN
	                        CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                    END IF
	    
	                    NBHD_HASH => TEMP_HASH%RIGHT%BACK%BOTTOM
	                    IF (ASSOCIATED(NBHD_HASH)) THEN
	                        CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                    END IF
	                END IF
	    
	                NBHD_HASH => TEMP_HASH%RIGHT%TOP
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                NBHD_HASH => TEMP_HASH%RIGHT%BOTTOM
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                NBHD_HASH => TEMP_HASH%RIGHT%FRONT
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                IF (ASSOCIATED(TEMP_HASH%RIGHT%FRONT)) THEN
	                    NBHD_HASH => TEMP_HASH%RIGHT%FRONT%TOP
	                    IF (ASSOCIATED(NBHD_HASH)) THEN
	                        CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                    END IF
	    
	                    NBHD_HASH => TEMP_HASH%RIGHT%FRONT%BOTTOM
	                    IF (ASSOCIATED(NBHD_HASH)) THEN
	                        CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                    END IF
	                END IF
	            END IF
	            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	            IF (ASSOCIATED(TEMP_HASH%BACK)) THEN
	                NBHD_HASH => TEMP_HASH%BACK%TOP
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                NBHD_HASH => TEMP_HASH%BACK%BOTTOM
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	            END IF

	            IF (ASSOCIATED(TEMP_HASH%FRONT)) THEN                    
	                NBHD_HASH => TEMP_HASH%FRONT%TOP
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	    
	                NBHD_HASH => TEMP_HASH%FRONT%BOTTOM
	                IF (ASSOCIATED(NBHD_HASH)) THEN
	                    CALL FIND_RELATEDPT_ITER(I, NBHD_HASH,  POINT_USED, RMIN, JMIN)
	                END IF
	            END IF
	            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	            IF(RMIN < RMIN_MAX) THEN
	                K = HASH0%TYP2 + 1
	                SURFACE_CURRENT1%POINT_RELATEDPT(K,I) = JMIN
	                !K = TYP1 + 1
	                !SURFACE_CURRENT2%POINT_RELATEDPT(K,JMIN) = I
	    
	                POINT_USED(JMIN) = .TRUE.
	            END IF
	        END IF
	    END DO
	    TEMP_HASH => TEMP_HASH%NEXT
        END DO

	CALL DELETE_HASH(HASH0)
	NULLIFY(NBHD_HASH)
        NULLIFY(TEMP_HASH)
        DEALLOCATE(POINT_USED)

	WRITE(STR1,*) TYP1
	WRITE(STR2,*) TYP2
	STR = './output/surface/point_relatedpt_3d_initial_' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'

	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
	    WRITE(21,*) SURFACE_CURRENT1%POINT_RELATEDPT(1+TYP2,I)
	END DO
	!$OMP END DO
	CLOSE(21)
else if(case1==2) then
	WRITE(STR1,*) TYP1
	WRITE(STR2,*) TYP2
	STR = './output/surface/point_relatedpt_3d_initial_' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'

	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
	    READ(21,*) SURFACE_CURRENT1%POINT_RELATEDPT(1+TYP2,I)
	END DO
	!$OMP END DO
	CLOSE(21)

end if
        NULLIFY(SURFACE_CURRENT1)
        NULLIFY(SURFACE_CURRENT2)

    END SUBROUTINE FIND_RELATEDPT

    
    
    SUBROUTINE FIND_RELATEDFACE_ITER(I, CURRENT_HASH, RMIN, JMIN)
	IMPLICIT NONE
        INTEGER :: I,J,K
        REAL(8) :: R
        INTEGER :: JMIN
        REAL(8) :: RMIN
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1, SURFACE_CURRENT2
        TYPE(HASH), POINTER :: CURRENT_HASH
        INTEGER :: N2
        
        IF (CURRENT_HASH%TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (CURRENT_HASH%TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (CURRENT_HASH%TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (CURRENT_HASH%TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
        
        K = CURRENT_HASH%TYP2 + 1
        DO N2 = 1, CURRENT_HASH%ELEMENT_NUM2
            J = CURRENT_HASH%ELEMENT2(N2)
            CALL UNSIGNED_DISTANCE_FACE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(1,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(2,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(3,J)),R)
            IF(R<RMIN) THEN
		IF((CURRENT_HASH%TYP1 == 1 .AND. CURRENT_HASH%TYP2 ==0) .OR. (CURRENT_HASH%TYP1 == 0 .AND. CURRENT_HASH%TYP2 ==2)) THEN
		    IF(SURFACE_CURRENT2%FACE_ONINTERFACE(J) .NE. -1) THEN
			RMIN = R
			JMIN = J
		    END IF
		ELSE
                    RMIN = R
                    JMIN = J
		END IF
            END IF
        END DO
        SURFACE_CURRENT1%POINT_RELATEDFACE(K,I) = JMIN

    END SUBROUTINE
    
    
    SUBROUTINE FIND_RELATEDFACE(TYP1, TYP2)
	IMPLICIT NONE
        INTEGER :: TYP1, TYP2
        INTEGER :: I,J,K,L,M, I1, I2, I3, I4, I5, I6, I7, J1,J2,J3,IDX,NUM
	INTEGER :: RIDGE_POINT_NUM, RIDGE_EDGE_NUM, FACE_INDEX !MINIDX1, MINIDX2, FACE_INDEX
        INTEGER :: JMIN, case1
	CHARACTER(500) :: STR, STR1, STR2
        REAL(8) :: RMIN, COORD, DIST, TEMPDIST
        LOGICAL :: B
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1, SURFACE_CURRENT2
        TYPE(HASH), POINTER :: CURRENT_HASH, TEMP_HASH, NBHD_HASH
        INTEGER :: N1, HASH_NUM1, HASH_NUM2, HASH_NUM3
	INTEGER, ALLOCATABLE :: RIDGE_EDGE(:,:)
	REAL(8) :: HASH_SIZE
	REAL(8) :: DIST1, DIST2, DIST3 !MINDIST
        IF (TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
      
case1 = 1

if(case1 ==1)  then
        K = TYP2 + 1
        SURFACE_CURRENT1%POINT_RELATEDFACE(K,:) = 0
        
        HASH_SIZE = 0.03
        CALL SETTING_HASH(HASH_SIZE, HASH_NUM1, HASH_NUM2, HASH_NUM3)
        NULLIFY(CURRENT_HASH)
        CALL GENERATE_HASH(TYP1,1,TYP2,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, CURRENT_HASH)

        TEMP_HASH=>CURRENT_HASH
        DO WHILE(ASSOCIATED(TEMP_HASH))
            DO N1 = 1, TEMP_HASH%ELEMENT_NUM1
                I = TEMP_HASH%ELEMENT1(N1)
                B = .FALSE.
                DO J=1,SURFACE_CURRENT1%POINT_FACE_CONNECTION_NUM(I)
                    I1 = SURFACE_CURRENT1%POINT_FACE_CONNECTION(J,I)
                    IF(SURFACE_CURRENT1%FACE_ONINTERFACE(I1)==TYP2) THEN
                        B = .TRUE.
                        EXIT
                    END IF
                END DO

                IF(B) THEN
                    JMIN = 0
                    RMIN = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1), DOMAIN_MAX(2)-DOMAIN_MIN(2), DOMAIN_MAX(3)-DOMAIN_MIN(3))
            
                    NBHD_HASH => TEMP_HASH
                    CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    
                    NBHD_HASH => TEMP_HASH%LEFT
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%RIGHT
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%BOTTOM
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%TOP
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%BACK
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%FRONT
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    IF (ASSOCIATED(TEMP_HASH%LEFT)) THEN              
                        NBHD_HASH => TEMP_HASH%LEFT%BACK
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF

                            IF (ASSOCIATED(TEMP_HASH%LEFT%BACK)) THEN 
                            NBHD_HASH => TEMP_HASH%LEFT%BACK%TOP
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                    
                            NBHD_HASH => TEMP_HASH%LEFT%BACK%BOTTOM
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%LEFT%TOP
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%LEFT%BOTTOM
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%LEFT%FRONT
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        IF (ASSOCIATED(TEMP_HASH%LEFT%FRONT)) THEN
                            NBHD_HASH => TEMP_HASH%LEFT%FRONT%TOP
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                    
                            NBHD_HASH => TEMP_HASH%LEFT%FRONT%BOTTOM
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                        END IF
                    END IF
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    IF (ASSOCIATED(TEMP_HASH%RIGHT)) THEN 
                        NBHD_HASH => TEMP_HASH%RIGHT%BACK
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        IF (ASSOCIATED(TEMP_HASH%RIGHT%BACK)) THEN
                            NBHD_HASH => TEMP_HASH%RIGHT%BACK%TOP
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                    
                            NBHD_HASH => TEMP_HASH%RIGHT%BACK%BOTTOM
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%RIGHT%TOP
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%RIGHT%BOTTOM
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%RIGHT%FRONT
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
       
                        IF (ASSOCIATED(TEMP_HASH%RIGHT%FRONT)) THEN             
                            NBHD_HASH => TEMP_HASH%RIGHT%FRONT%TOP
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                    
                            NBHD_HASH => TEMP_HASH%RIGHT%FRONT%BOTTOM
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                        END IF
                    END IF
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    IF (ASSOCIATED(TEMP_HASH%BACK)) THEN
                        NBHD_HASH => TEMP_HASH%BACK%TOP
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%BACK%BOTTOM
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    END IF

                    IF (ASSOCIATED(TEMP_HASH%FRONT)) THEN
                        NBHD_HASH => TEMP_HASH%FRONT%TOP
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%FRONT%BOTTOM
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    END IF
                
                END IF
	        END DO
            TEMP_HASH => TEMP_HASH%NEXT
        END DO
        
        CALL DELETE_HASH(CURRENT_HASH)
        NULLIFY(TEMP_HASH)
        NULLIFY(NBHD_HASH)

	IF(.NOT.(TYP1 .EQ. 0 .AND. TYP2 .EQ. 2) .AND. .NOT. (TYP1 .EQ. 1 .AND. TYP2 .EQ. 2) .AND. .NOT. (TYP1 .EQ. 1 .AND. TYP2 .EQ. 0)) THEN
        DO I=1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
            IF(SURFACE_CURRENT1%POINT_RELATEDFACE(K,I).NE.0) THEN
		RIDGE_POINT_NUM = 0
		RIDGE_EDGE_NUM = 0
		DIST1 = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1),DOMAIN_MAX(2)-DOMAIN_MIN(2),DOMAIN_MAX(3)-DOMAIN_MIN(3))
		DIST2 = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1),DOMAIN_MAX(2)-DOMAIN_MIN(2),DOMAIN_MAX(3)-DOMAIN_MIN(3))
		DIST3 = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1),DOMAIN_MAX(2)-DOMAIN_MIN(2),DOMAIN_MAX(3)-DOMAIN_MIN(3))
                J = SURFACE_CURRENT1%POINT_RELATEDFACE(K,I)
		I2 = SURFACE_CURRENT2%SURFACE_FACES(1,J)
		I3 = SURFACE_CURRENT2%SURFACE_FACES(2,J)
		I4 = SURFACE_CURRENT2%SURFACE_FACES(3,J)

		IF(SURFACE_CURRENT1%POINT_TYPE(I) .NE. 2) THEN
		    CALL PROJECTION_FACE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(1,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(2,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(3,J)))
		ELSE
		    ALLOCATE(RIDGE_EDGE(2,100))
		    RIDGE_EDGE_NUM = 0
		    DO L= 1,3
			I5 = SURFACE_CURRENT2%SURFACE_FACES(L,J)
			NUM = SURFACE_CURRENT2%POINT_FACE_CONNECTION_NUM(I5)
			    DO M= 1,NUM
			    	FACE_INDEX = SURFACE_CURRENT2%POINT_FACE_CONNECTION(M,I5)
				J1 = SURFACE_CURRENT2%SURFACE_FACES(1,FACE_INDEX)
				J2 = SURFACE_CURRENT2%SURFACE_FACES(2,FACE_INDEX)
				J3 = SURFACE_CURRENT2%SURFACE_FACES(3,FACE_INDEX)
				IF(SURFACE_CURRENT2%POINT_TYPE(J1)==2 .AND. SURFACE_CURRENT2%POINT_TYPE(J2)==2) THEN
				    RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
				    RIDGE_EDGE(1,RIDGE_EDGE_NUM) = J1
				    RIDGE_EDGE(2,RIDGE_EDGE_NUM) = J2
				END IF
				IF(SURFACE_CURRENT2%POINT_TYPE(J2)==2 .AND. SURFACE_CURRENT2%POINT_TYPE(J3)==2) THEN
				    RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
				    RIDGE_EDGE(1,RIDGE_EDGE_NUM) = J2
				    RIDGE_EDGE(2,RIDGE_EDGE_NUM) = J3
				END IF
				IF(SURFACE_CURRENT2%POINT_TYPE(J3)==2 .AND. SURFACE_CURRENT2%POINT_TYPE(J1)==2) THEN
				    RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
				    RIDGE_EDGE(1,RIDGE_EDGE_NUM) = J3
				    RIDGE_EDGE(2,RIDGE_EDGE_NUM) = J1
				END IF
			    END DO
		    END DO

		    IF(RIDGE_EDGE_NUM==0) THEN
			CALL PROJECTION_FACE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(1,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(2,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(3,J)))
		    ELSE
			DIST = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1), DOMAIN_MAX(2)-DOMAIN_MIN(2), DOMAIN_MAX(3)-DOMAIN_MIN(3))
			DO L=1,RIDGE_EDGE_NUM
			    I6 = RIDGE_EDGE(1,L)
			    I7 = RIDGE_EDGE(2,L)
			    CALL UNSIGNED_DISTANCE_EDGE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,I6),SURFACE_CURRENT2%SURFACE_POINTS(:,I7),TEMPDIST)
			    IF(TEMPDIST<DIST) THEN
				DIST = TEMPDIST
				IDX = L
			    END IF
		        END DO
			I6 = RIDGE_EDGE(1,IDX)
			I7 = RIDGE_EDGE(2,IDX)
			CALL PROJECTION_EDGE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,I6),SURFACE_CURRENT2%SURFACE_POINTS(:,I7),COORD)
		    END IF
		    DEALLOCATE(RIDGE_EDGE)
!		    IF(SURFACE_CURRENT2%POINT_TYPE(I2) == 2) THEN
!		        RIDGE_POINT_NUM = RIDGE_POINT_NUM + 1
!			IDX = I2
!		    END IF	
	
!		    IF(SURFACE_CURRENT2%POINT_TYPE(I3) == 2) THEN
!		        RIDGE_POINT_NUM = RIDGE_POINT_NUM + 1
!			IDX = I3
!		    END IF

!		    IF(SURFACE_CURRENT2%POINT_TYPE(I4) == 2) THEN
!		        RIDGE_POINT_NUM = RIDGE_POINT_NUM + 1
!			IDX = I4
!		    END IF    

!		    IF(SURFACE_CURRENT2%POINT_TYPE(I2) == 2 .AND. SURFACE_CURRENT2%POINT_TYPE(I3) == 2) THEN
!		        CALL UNSIGNED_DISTANCE_EDGE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,I2),SURFACE_CURRENT2%SURFACE_POINTS(:,I3),DIST1)
!		        RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
!		    END IF

!		    IF(SURFACE_CURRENT2%POINT_TYPE(I3) == 2 .AND. SURFACE_CURRENT2%POINT_TYPE(I4) == 2) THEN
!		        CALL UNSIGNED_DISTANCE_EDGE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,I3),SURFACE_CURRENT2%SURFACE_POINTS(:,I4),DIST2)
!		        RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
!		    END IF

!		    IF(SURFACE_CURRENT2%POINT_TYPE(I4) == 2 .AND. SURFACE_CURRENT2%POINT_TYPE(I2) == 2) THEN 
!		        CALL UNSIGNED_DISTANCE_EDGE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,I4),SURFACE_CURRENT2%SURFACE_POINTS(:,I2),DIST3)
!		        RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
!		    END IF

!		    IF(RIDGE_EDGE_NUM==0 .AND. RIDGE_POINT_NUM==1) THEN
!			SURFACE_CURRENT1%SURFACE_POINTS(:,I) = SURFACE_CURRENT2%SURFACE_POINTS(:,IDX)
!		    ELSEIF(RIDGE_EDGE_NUM .GE. 1) THEN
!			DO L=1,3
!			    IF(L==1) THEN
!			    MINDIST = DIST1
!			    MINIDX1 = I2
!			    MINIDX2 = I3
!			    ELSEIF(L==2) THEN
!				IF(DIST2<MINDIST) THEN
!				MINDIST = DIST2
!				MINIDX1 = I3
!				MINIDX2 = I4
!				END IF
!			    ELSE
!				IF(DIST3<MINDIST) THEN
!				MINDIST = DIST3
!				MINIDX1 = I4
!				MINIDX2 = I2
!				END IF
!			    END IF
!		        END DO
!		    CALL PROJECTION_EDGE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,MINIDX1),SURFACE_CURRENT2%SURFACE_POINTS(:,MINIDX2),COORD)
!		    END IF	
		END IF			
            END IF

        END DO
	END IF

!        DO I=1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
!            IF(SURFACE_CURRENT1%POINT_RELATEDFACE(K,I).NE.0) THEN
!                J = SURFACE_CURRENT1%POINT_RELATEDFACE(K,I)
!                CALL PROJECTION_FACE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(1,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(2,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(3,J)))
!            END IF
!        END DO
	WRITE(STR1,*) TYP1
	WRITE(STR2,*) TYP2
	STR = './output/surface/point_relatedface_3d_initial_' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'

	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
	    WRITE(21,*) SURFACE_CURRENT1%POINT_RELATEDFACE(1+TYP2,I)
	END DO
	!$OMP END DO
	CLOSE(21)

else if(case1==2) then

	WRITE(STR1,*) TYP1
	WRITE(STR2,*) TYP2
	STR = './output/surface/point_relatedface_3d_initial_' // TRIM(ADJUSTL(STR1)) // '_' // TRIM(ADJUSTL(STR2)) // '.txt'

	OPEN(UNIT=21, FILE = STR)
	!$OMP DO ORDERED 
	DO I = 1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
	    READ(21,*) SURFACE_CURRENT1%POINT_RELATEDFACE(1+TYP2,I)
	END DO
	!$OMP END DO
	CLOSE(21)
END IF        

        NULLIFY(SURFACE_CURRENT1)
        NULLIFY(SURFACE_CURRENT2)
    END SUBROUTINE FIND_RELATEDFACE

    SUBROUTINE UPDATE_RELATEDFACE(TYP1, TYP2, ISPROJ)
	IMPLICIT NONE
        INTEGER :: TYP1, TYP2
        INTEGER :: I,J,K,L,M, I1, I2, I3, I4, I5, I6, I7, J1,J2,J3,IDX,NUM
	INTEGER :: RIDGE_POINT_NUM, RIDGE_EDGE_NUM, MINIDX1, MINIDX2, FACE_INDEX
        INTEGER :: JMIN
        REAL(8) :: RMIN, COORD, DIST, TEMPDIST
        LOGICAL :: B, ISPROJ
        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1, SURFACE_CURRENT2
        TYPE(HASH), POINTER :: CURRENT_HASH, TEMP_HASH, NBHD_HASH
        INTEGER :: N1, HASH_NUM1, HASH_NUM2, HASH_NUM3
	INTEGER, ALLOCATABLE :: RIDGE_EDGE(:,:)
	REAL(8) :: HASH_SIZE
	REAL(8) :: DIST1, DIST2, DIST3, MINDIST
        IF (TYP1==0) THEN
            SURFACE_CURRENT1 => SURFACE_FLUID
        END IF
        IF (TYP1==1) THEN
            SURFACE_CURRENT1 => SURFACE_PROPEL
        END IF
        IF (TYP1==2) THEN
            SURFACE_CURRENT1 => SURFACE_CASE
        END IF
        
        IF (TYP2==0) THEN
            SURFACE_CURRENT2 => SURFACE_FLUID
        END IF
        IF (TYP2==1) THEN
            SURFACE_CURRENT2 => SURFACE_PROPEL
        END IF
        IF (TYP2==2) THEN
            SURFACE_CURRENT2 => SURFACE_CASE
        END IF
        
        K = TYP2 + 1
        SURFACE_CURRENT1%POINT_RELATEDFACE(K,:) = 0
        
        HASH_SIZE = 0.03
        CALL SETTING_HASH(HASH_SIZE, HASH_NUM1, HASH_NUM2, HASH_NUM3)
        NULLIFY(CURRENT_HASH)
        CALL GENERATE_HASH(TYP1,1,TYP2,2, 1, 1, 1, HASH_NUM1, HASH_NUM2, HASH_NUM3, HASH_SIZE, CURRENT_HASH)

        TEMP_HASH=>CURRENT_HASH
        DO WHILE(ASSOCIATED(TEMP_HASH))
            DO N1 = 1, TEMP_HASH%ELEMENT_NUM1
                I = TEMP_HASH%ELEMENT1(N1)
                B = .FALSE.
                DO J=1,SURFACE_CURRENT1%POINT_FACE_CONNECTION_NUM(I)
                    I1 = SURFACE_CURRENT1%POINT_FACE_CONNECTION(J,I)
                    IF(SURFACE_CURRENT1%FACE_ONINTERFACE(I1)==TYP2) THEN
                        B = .TRUE.
                        EXIT
                    END IF
                END DO

                IF(B) THEN
                    JMIN = 0
                    RMIN = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1), DOMAIN_MAX(2)-DOMAIN_MIN(2), DOMAIN_MAX(3)-DOMAIN_MIN(3))
            
                    NBHD_HASH => TEMP_HASH
                    CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    
                    NBHD_HASH => TEMP_HASH%LEFT
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%RIGHT
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%BOTTOM
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%TOP
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%BACK
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    
                    NBHD_HASH => TEMP_HASH%FRONT
                    IF (ASSOCIATED(NBHD_HASH)) THEN
                        CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                    END IF
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    IF (ASSOCIATED(TEMP_HASH%LEFT)) THEN              
                        NBHD_HASH => TEMP_HASH%LEFT%BACK
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF

                            IF (ASSOCIATED(TEMP_HASH%LEFT%BACK)) THEN 
                            NBHD_HASH => TEMP_HASH%LEFT%BACK%TOP
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                    
                            NBHD_HASH => TEMP_HASH%LEFT%BACK%BOTTOM
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%LEFT%TOP
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%LEFT%BOTTOM
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%LEFT%FRONT
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        IF (ASSOCIATED(TEMP_HASH%LEFT%FRONT)) THEN
                            NBHD_HASH => TEMP_HASH%LEFT%FRONT%TOP
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                    
                            NBHD_HASH => TEMP_HASH%LEFT%FRONT%BOTTOM
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                        END IF
                    END IF
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    IF (ASSOCIATED(TEMP_HASH%RIGHT)) THEN 
                        NBHD_HASH => TEMP_HASH%RIGHT%BACK
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        IF (ASSOCIATED(TEMP_HASH%RIGHT%BACK)) THEN
                            NBHD_HASH => TEMP_HASH%RIGHT%BACK%TOP
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                    
                            NBHD_HASH => TEMP_HASH%RIGHT%BACK%BOTTOM
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%RIGHT%TOP
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%RIGHT%BOTTOM
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%RIGHT%FRONT
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
       
                        IF (ASSOCIATED(TEMP_HASH%RIGHT%FRONT)) THEN             
                            NBHD_HASH => TEMP_HASH%RIGHT%FRONT%TOP
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                    
                            NBHD_HASH => TEMP_HASH%RIGHT%FRONT%BOTTOM
                            IF (ASSOCIATED(NBHD_HASH)) THEN
                                CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                            END IF
                        END IF
                    END IF
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    IF (ASSOCIATED(TEMP_HASH%BACK)) THEN
                        NBHD_HASH => TEMP_HASH%BACK%TOP
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%BACK%BOTTOM
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    END IF

                    IF (ASSOCIATED(TEMP_HASH%FRONT)) THEN
                        NBHD_HASH => TEMP_HASH%FRONT%TOP
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    
                        NBHD_HASH => TEMP_HASH%FRONT%BOTTOM
                        IF (ASSOCIATED(NBHD_HASH)) THEN
                            CALL FIND_RELATEDFACE_ITER(I, NBHD_HASH, RMIN, JMIN)
                        END IF
                    END IF
                
                END IF
	        END DO
            TEMP_HASH => TEMP_HASH%NEXT
        END DO
        
        CALL DELETE_HASH(CURRENT_HASH)
        NULLIFY(TEMP_HASH)
        NULLIFY(NBHD_HASH)

	IF(ISPROJ) THEN
        DO I=1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
            IF(SURFACE_CURRENT1%POINT_RELATEDFACE(K,I) .NE. 0) THEN
		
        RIDGE_POINT_NUM = 0
		RIDGE_EDGE_NUM = 0
		DIST1 = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1),DOMAIN_MAX(2)-DOMAIN_MIN(2),DOMAIN_MAX(3)-DOMAIN_MIN(3))
		DIST2 = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1),DOMAIN_MAX(2)-DOMAIN_MIN(2),DOMAIN_MAX(3)-DOMAIN_MIN(3))
		DIST3 = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1),DOMAIN_MAX(2)-DOMAIN_MIN(2),DOMAIN_MAX(3)-DOMAIN_MIN(3))
        
        J = SURFACE_CURRENT1%POINT_RELATEDFACE(K,I)
		I2 = SURFACE_CURRENT2%SURFACE_FACES(1,J)
		I3 = SURFACE_CURRENT2%SURFACE_FACES(2,J)
		I4 = SURFACE_CURRENT2%SURFACE_FACES(3,J)

		    IF(SURFACE_CURRENT1%POINT_TYPE(I) == 1) THEN
		        CALL PROJECTION_FACE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(1,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(2,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(3,J)))
		
            	    ELSEIF(SURFACE_CURRENT1%POINT_TYPE(I)==2) THEN
		        ALLOCATE(RIDGE_EDGE(2,100))
			IF(SURFACE_CURRENT2%POINT_TYPE(I2) .GE. 2 .AND. SURFACE_CURRENT2%POINT_TYPE(I3) .GE. 2) THEN
			     RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
			     RIDGE_EDGE(1,RIDGE_EDGE_NUM) = I2
			     RIDGE_EDGE(2,RIDGE_EDGE_NUM) = I3
			END IF
			IF(SURFACE_CURRENT2%POINT_TYPE(I3) .GE. 2 .AND. SURFACE_CURRENT2%POINT_TYPE(I4) .GE. 2) THEN
			     RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
			     RIDGE_EDGE(1,RIDGE_EDGE_NUM) = I3
			     RIDGE_EDGE(2,RIDGE_EDGE_NUM) = I4
			END IF
			IF(SURFACE_CURRENT2%POINT_TYPE(I4) .GE. 2 .AND. SURFACE_CURRENT2%POINT_TYPE(I2) .GE. 2) THEN
			     RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
			     RIDGE_EDGE(1,RIDGE_EDGE_NUM) = I4
			     RIDGE_EDGE(2,RIDGE_EDGE_NUM) = I2
			END IF			

			IF(.FALSE.) THEN
		        DO L= 1,3
			    I5 = SURFACE_CURRENT2%SURFACE_FACES(L,J)
			    NUM = SURFACE_CURRENT2%POINT_FACE_CONNECTION_NUM(I5)
			        DO M= 1,NUM
			    	    FACE_INDEX = SURFACE_CURRENT2%POINT_FACE_CONNECTION(M,I5)
				    J1 = SURFACE_CURRENT2%SURFACE_FACES(1,FACE_INDEX)
				    J2 = SURFACE_CURRENT2%SURFACE_FACES(2,FACE_INDEX)
				    J3 = SURFACE_CURRENT2%SURFACE_FACES(3,FACE_INDEX)
                    
				    IF(SURFACE_CURRENT2%POINT_TYPE(J1) .GE. 2) THEN
				        RIDGE_POINT_NUM = RIDGE_POINT_NUM + 1
				    END IF
				    
				    IF(SURFACE_CURRENT2%POINT_TYPE(J2) .GE. 2) THEN
				        RIDGE_POINT_NUM = RIDGE_POINT_NUM + 1
				    END IF

				    IF(SURFACE_CURRENT2%POINT_TYPE(J3) .GE. 2) THEN
				        RIDGE_POINT_NUM = RIDGE_POINT_NUM + 1
				    END IF
                    
				    IF(SURFACE_CURRENT2%POINT_TYPE(J1) .GE. 2 .AND. SURFACE_CURRENT2%POINT_TYPE(J2) .GE. 2) THEN
				        RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
				        RIDGE_EDGE(1,RIDGE_EDGE_NUM) = J1
				        RIDGE_EDGE(2,RIDGE_EDGE_NUM) = J2
				    END IF
				    IF(SURFACE_CURRENT2%POINT_TYPE(J2) .GE. 2 .AND. SURFACE_CURRENT2%POINT_TYPE(J3) .GE. 2) THEN
				        RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
				        RIDGE_EDGE(1,RIDGE_EDGE_NUM) = J2
				        RIDGE_EDGE(2,RIDGE_EDGE_NUM) = J3
				    END IF
				    IF(SURFACE_CURRENT2%POINT_TYPE(J3) .GE. 2 .AND. SURFACE_CURRENT2%POINT_TYPE(J1) .GE. 2) THEN
				        RIDGE_EDGE_NUM = RIDGE_EDGE_NUM + 1
				        RIDGE_EDGE(1,RIDGE_EDGE_NUM) = J3
				        RIDGE_EDGE(2,RIDGE_EDGE_NUM) = J1
				    END IF
			        END DO
		        END DO
			END IF

		        IF(RIDGE_EDGE_NUM==0) THEN
                    
                    	     WRITE(*,*) 'NO CORRESPONDING RIDGE EDGES FOR PROPEL PT TYP==2'
    			    CALL PROJECTION_FACE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(1,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(2,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(3,J)))
 
		        ELSE
			    DIST = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1), DOMAIN_MAX(2)-DOMAIN_MIN(2), DOMAIN_MAX(3)-DOMAIN_MIN(3))
			    DO L=1,RIDGE_EDGE_NUM
			        I6 = RIDGE_EDGE(1,L)
			        I7 = RIDGE_EDGE(2,L)
			        CALL UNSIGNED_DISTANCE_EDGE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,I6),SURFACE_CURRENT2%SURFACE_POINTS(:,I7),TEMPDIST)
			        IF(TEMPDIST<DIST) THEN
				    DIST = TEMPDIST
				    IDX = L
			        END IF
		            END DO
			    I6 = RIDGE_EDGE(1,IDX)
			    I7 = RIDGE_EDGE(2,IDX)
			    CALL PROJECTION_EDGE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,I6),SURFACE_CURRENT2%SURFACE_POINTS(:,I7),COORD)
		        END IF
		        DEALLOCATE(RIDGE_EDGE)
		    END IF			
            END IF
        END DO
	END IF

        NULLIFY(SURFACE_CURRENT1)
        NULLIFY(SURFACE_CURRENT2)
        
    END SUBROUTINE UPDATE_RELATEDFACE


    
!    SUBROUTINE UPDATE_RELATEDFACE(TYP1, TYP2)
!	IMPLICIT NONE
!       INTEGER :: TYP1, TYP2
!        INTEGER :: I,J,K,N, I1
!        REAL(8) :: R
!        INTEGER :: JMIN
!        REAL(8) :: RMIN
!        INTEGER :: J0,L
!        TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT1, SURFACE_CURRENT2
!        INTEGER, ALLOCATABLE :: TEMP_RELATEDFACE(:)
        

!        IF (TYP1==0) THEN
!            SURFACE_CURRENT1 => SURFACE_FLUID
!        END IF
!        IF (TYP1==1) THEN
!            SURFACE_CURRENT1 => SURFACE_PROPEL
!        END IF
!        IF (TYP1==2) THEN
!            SURFACE_CURRENT1 => SURFACE_CASE
!        END IF
        
!        IF (TYP2==0) THEN
!            SURFACE_CURRENT2 => SURFACE_FLUID
!        END IF
!        IF (TYP2==1) THEN
!            SURFACE_CURRENT2 => SURFACE_PROPEL
!        END IF
!        IF (TYP2==2) THEN
!            SURFACE_CURRENT2 => SURFACE_CASE
!        END IF
        
!        K = TYP2 + 1
        
!        ALLOCATE(TEMP_RELATEDFACE(SURFACE_CURRENT1%SURFACE_POINTS_NUM))
!        TEMP_RELATEDFACE = 0
        
!        DO I=1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
            
!            IF(SURFACE_CURRENT1%POINT_RELATEDFACE(K,I).NE.0) THEN
!                JMIN = 0
!                RMIN = MAX(DOMAIN_MAX(1)-DOMAIN_MIN(1), DOMAIN_MAX(2)-DOMAIN_MIN(2), DOMAIN_MAX(3)-DOMAIN_MIN(3))
                
!                J0 = SURFACE_CURRENT1%POINT_RELATEDFACE(K,I)
                
!                DO L=1,3
!                    I1 = SURFACE_CURRENT2%SURFACE_FACES(L,J0)
!                    DO N=1,SURFACE_CURRENT2%POINT_FACE_CONNECTION_NUM(I1)
!                        J = SURFACE_CURRENT2%POINT_FACE_CONNECTION(N,I1)
                    
!                        CALL UNSIGNED_DISTANCE_FACE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(1,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(2,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(3,J)),R)
!                        IF(R<RMIN) THEN
!                            RMIN = R
!                            JMIN = J
!                        END IF
!                    END DO
!                END DO
                
!                TEMP_RELATEDFACE(I) = JMIN
                
!            END IF
!        END DO
        
        !$OMP PARALLEL DO PRIVATE(I)
!        DO I=1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
!            SURFACE_CURRENT1%POINT_RELATEDFACE(K,I) = TEMP_RELATEDFACE(I)
!        END DO
        !$OMP END PARALLEL DO
        
!        DEALLOCATE(TEMP_RELATEDFACE)
        
        !$OMP PARALLEL DO PRIVATE(I,J)
!        DO I=1, SURFACE_CURRENT1%SURFACE_POINTS_NUM
!            IF(SURFACE_CURRENT1%POINT_RELATEDFACE(K,I).NE.0) THEN
!                J = SURFACE_CURRENT1%POINT_RELATEDFACE(K,I)
!                CALL PROJECTION_FACE_POINT(SURFACE_CURRENT1%SURFACE_POINTS(:,I),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(1,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(2,J)),SURFACE_CURRENT2%SURFACE_POINTS(:,SURFACE_CURRENT2%SURFACE_FACES(3,J)))
!            END IF
!        END DO
        !$OMP END PARALLEL DO
        
!    END SUBROUTINE UPDATE_RELATEDFACE
    
    SUBROUTINE SAVING_COEFFS_SURFACE_STRUCT(FACE_AREA, CORNER_INDEX)
	IMPLICIT NONE
        INTEGER, ALLOCATABLE :: CORNER_INDEX(:)
        REAL(8), ALLOCATABLE :: FACE_AREA(:)

	IF(ALLOCATED(CORNER_INDEX)) THEN
	    DEALLOCATE(CORNER_INDEX)
	END IF
	IF(ALLOCATED(FACE_AREA)) THEN
	    DEALLOCATE(FACE_AREA)
	END IF

	ALLOCATE(CORNER_INDEX(SURFACE_PROPEL%SURFACE_POINTS_NUM))
	ALLOCATE(FACE_AREA(SURFACE_PROPEL%SURFACE_FACES_NUM))

	CORNER_INDEX = SURFACE_PROPEL%POINT_TYPE   
        
    END SUBROUTINE SAVING_COEFFS_SURFACE_STRUCT
         

    SUBROUTINE LOADING_COEFFS_SURFACE_STRUCT(NEW_PROPEL_POINT_NUM, NEW_PROPEL_POINT, NEW_PROPEL_FACE_NUM, NEW_PROPEL_FACE, NEW_PROPEL_LOC, NEW_PROPEL_ONINTERFACE, NEW_PROPEL_CONNECTION)
        IMPLICIT NONE
        INTEGER :: NEW_PROPEL_POINT_NUM
        REAL(8) :: NEW_PROPEL_POINT(3,NEW_PROPEL_POINT_NUM)
        INTEGER :: NEW_PROPEL_FACE_NUM
        INTEGER :: NEW_PROPEL_FACE(4,NEW_PROPEL_FACE_NUM)
        INTEGER :: NEW_PROPEL_LOC(NEW_PROPEL_FACE_NUM)
        
        INTEGER :: NEW_PROPEL_CONNECTION(:,:)
        INTEGER :: NEW_PROPEL_ONINTERFACE(:)
NEW_PROPEL_POINT_NUM = 0
NEW_PROPEL_POINT = 0
NEW_PROPEL_FACE_NUM = 0 
NEW_PROPEL_FACE =0
NEW_PROPEL_LOC=0
NEW_PROPEL_CONNECTION=0
NEW_PROPEL_ONINTERFACE =0 
    END SUBROUTINE LOADING_COEFFS_SURFACE_STRUCT
    
END MODULE OPERATORS_3D
