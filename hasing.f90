


MODULE HASHING_3D

    IMPLICIT NONE

    TYPE HASH
        REAL(8) :: X0, X1, Y0, Y1, Z0, Z1
        INTEGER :: X_INDEX, Y_INDEX, Z_INDEX
        
        TYPE(HASH), POINTER :: NEXT
        TYPE(HASH), POINTER :: BEFORE
        
        TYPE(HASH), POINTER :: LEFT
        TYPE(HASH), POINTER :: RIGHT
        TYPE(HASH), POINTER :: FRONT
        TYPE(HASH), POINTER :: BACK
        TYPE(HASH), POINTER :: BOTTOM
        TYPE(HASH), POINTER :: TOP
        
        INTEGER, ALLOCATABLE :: FLUID_POINT_INDEX1(:)
        INTEGER, ALLOCATABLE :: PROPEL_POINT_INDEX1(:)
        INTEGER, ALLOCATABLE :: CASE_POINT_INDEX1(:)
        INTEGER, ALLOCATABLE :: FLUID_FACE_INDEX1(:)
        INTEGER, ALLOCATABLE :: PROPEL_FACE_INDEX1(:)
        INTEGER, ALLOCATABLE :: CASE_FACE_INDEX1(:)   
        
        INTEGER, ALLOCATABLE :: FLUID_POINT_INDEX2(:)
        INTEGER, ALLOCATABLE :: PROPEL_POINT_INDEX2(:)
        INTEGER, ALLOCATABLE :: CASE_POINT_INDEX2(:)
        INTEGER, ALLOCATABLE :: FLUID_FACE_INDEX2(:)
        INTEGER, ALLOCATABLE :: PROPEL_FACE_INDEX2(:)
        INTEGER, ALLOCATABLE :: CASE_FACE_INDEX2(:)  
    
    END TYPE
    
    CONTAINS
    
    SUBROUTINE INITIALIZATION_HASH(TYP1, ELEMENT_TYP1, TYP2, ELEMENT_TYP2,I,J,K, HASH_SIZE, CURRENT_HASH)
        INTEGER :: I, J, K, TYP1, ELEMENT_TYP1,  TYP2, ELEMENT_TYP2
        REAL(8) :: HASH_SIZE
        !TYPE(SURFACE_TYPE), POINTER :: SURFACE_CURRENT
        TYPE(HASH), POINTER :: CURRENT_HASH
        INTEGER :: TEMP_ELEMENT_NUM
        
        
        ALLOCATE(CURRENT_HASH)
        
        NULLIFY(CURRENT_HASH%NEXT)
        NULLIFY(CURRENT_HASH%BEFORE)
        NULLIFY(CURRENT_HASH%LEFT)
        NULLIFY(CURRENT_HASH%RIGHT)
        NULLIFY(CURRENT_HASH%FRONT)
        NULLIFY(CURRENT_HASH%BACK)
        NULLIFY(CURRENT_HASH%BOTTOM)
        NULLIFY(CURRENT_HASH%TOP)
        
        CURRENT_HASH%X_INDEX = I
        CURRENT_HASH%Y_INDEX = J
        CURRENT_HASH%Z_INDEX = K
        
        CURRENT_HASH%X0 = HASH_SIZE*REAL(I-1)
        CURRENT_HASH%X1 = HASH_SIZE*REAL(I)
        CURRENT_HASH%Y0 = HASH_SIZE*REAL(J-1)
        CURRENT_HASH%Y1 = HASH_SIZE*REAL(J)
        CURRENT_HASH%Z0 = HASH_SIZE*REAL(K-1)
        CURRENT_HASH%Z1 = HASH_SIZE*REAL(K)
    END SUBROUTINE
    

    
    SUBROUTINE DELETE_HASH(GIVEN_HASH)
        TYPE(HASH), POINTER :: GIVEN_HASH
        TYPE(HASH), POINTER :: CURRENT_HASH
        TYPE(HASH), POINTER :: TEMP_HASH
        
        CURRENT_HASH=>GIVEN_HASH
        DO WHILE (ASSOCIATED(CURRENT_HASH))
            TEMP_HASH=>CURRENT_HASH%NEXT
            if (ALLOCATED(CURRENT_HASH%FLUID_POINT_INDEX1)) THEN
                DEALLOCATE(CURRENT_HASH%FLUID_POINT_INDEX1)
            END IF
            if (ALLOCATED(CURRENT_HASH%PROPEL_POINT_INDEX1)) THEN
                DEALLOCATE(CURRENT_HASH%PROPEL_POINT_INDEX1)
            END IF
            if (ALLOCATED(CURRENT_HASH%CASE_POINT_INDEX1)) THEN
                DEALLOCATE(CURRENT_HASH%CASE_POINT_INDEX1)
            END IF
            if (ALLOCATED(CURRENT_HASH%FLUID_FACE_INDEX1)) THEN
                DEALLOCATE(CURRENT_HASH%FLUID_FACE_INDEX1)
            END IF
            if (ALLOCATED(CURRENT_HASH%PROPEL_FACE_INDEX1)) THEN
                DEALLOCATE(CURRENT_HASH%PROPEL_FACE_INDEX1)
            END IF
            if (ALLOCATED(CURRENT_HASH%CASE_FACE_INDEX1)) THEN
                DEALLOCATE(CURRENT_HASH%CASE_FACE_INDEX1)
            END IF
            
            if (ALLOCATED(CURRENT_HASH%FLUID_POINT_INDEX2)) THEN
                DEALLOCATE(CURRENT_HASH%FLUID_POINT_INDEX2)
            END IF
            if (ALLOCATED(CURRENT_HASH%PROPEL_POINT_INDEX2)) THEN
                DEALLOCATE(CURRENT_HASH%PROPEL_POINT_INDEX2)
            END IF
            if (ALLOCATED(CURRENT_HASH%CASE_POINT_INDEX2)) THEN
                DEALLOCATE(CURRENT_HASH%CASE_POINT_INDEX2)
            END IF
            if (ALLOCATED(CURRENT_HASH%FLUID_FACE_INDEX2)) THEN
                DEALLOCATE(CURRENT_HASH%FLUID_FACE_INDEX2)
            END IF
            if (ALLOCATED(CURRENT_HASH%PROPEL_FACE_INDEX2)) THEN
                DEALLOCATE(CURRENT_HASH%PROPEL_FACE_INDEX2)
            END IF
            if (ALLOCATED(CURRENT_HASH%CASE_FACE_INDEX2)) THEN
                DEALLOCATE(CURRENT_HASH%CASE_FACE_INDEX2)
            END IF
            
            DEALLOCATE(CURRENT_HASH)
            NULLIFY(CURRENT_HASH)
            CURRENT_HASH=>TEMP_HASH
        END DO
    END SUBROUTINE

    
    
    
    RECURSIVE SUBROUTINE GENERATE_HASH(typ1,ELEMENT_TYP1, typ2,ELEMENT_TYP2, i, j, k, HASH_NUM, HASH_SIZE, CURRENT_HASH)
        INTEGER :: I, J, K, TYP1, ELEMENT_TYP1,  TYP2, ELEMENT_TYP2    
        REAL(8) :: HASH_SIZE
        INTEGER :: NUM, HASH_NUM
        TYPE(HASH), POINTER :: CURRENT_HASH
        TYPE(HASH), POINTER :: NEW_HASH
        
        if (.NOT.ASSOCIATED(CURRENT_HASH)) THEN
            CALL INITIALIZATION_HASH(typ1,ELEMENT_TYP1,typ2,ELEMENT_TYP1, I, J, K, HASH_SIZE, CURRENT_HASH)
        END IF
        
        
        IF (I-1<1) THEN
            CURRENT_HASH%LEFT => NULL()
            !RETURN
        ELSE IF (.NOT. ASSOCIATED(CURRENT_HASH%LEFT)) THEN
            CALL FIND_HASH(I-1, J, K, CURRENT_HASH, NEW_HASH)
            IF (ASSOCIATED(NEW_HASH)) THEN
                CURRENT_HASH%LEFT => NEW_HASH
                NEW_HASH%RIGHT=>CURRENT_HASH
            ELSE
                CALL INITIALIZATION_HASH(TYP1,ELEMENT_TYP1,TYP2,ELEMENT_TYP1, I-1, J, K, HASH_SIZE, CURRENT_HASH%LEFT)
                CURRENT_HASH%LEFT%RIGHT=>CURRENT_HASH
                CALL CONNECTED_IN_A_ROW(CURRENT_HASH, CURRENT_HASH%LEFT)
                CALL GENERATE_HASH(TYP1, ELEMENT_TYP1, TYP2, ELEMENT_TYP1, I-1, J, K, HASH_NUM, HASH_SIZE, CURRENT_HASH%LEFT)
            END IF
        END IF
        
        IF (I+1>HASH_NUM) THEN
            CURRENT_HASH%RIGHT => NULL()
        ELSE IF (.NOT. ASSOCIATED(CURRENT_HASH%RIGHT)) THEN
            CALL FIND_HASH(I+1, J, K, CURRENT_HASH, NEW_HASH)
            IF (ASSOCIATED(NEW_HASH)) THEN
                CURRENT_HASH%RIGHT => NEW_HASH
                NEW_HASH%LEFT => CURRENT_HASH
            ELSE
                CALL INITIALIZATION_HASH(TYP1,ELEMENT_TYP1,TYP2,ELEMENT_TYP1, I+1, J, K, HASH_SIZE, CURRENT_HASH%RIGHT)
                CURRENT_HASH%RIGHT%LEFT => CURRENT_HASH
                CALL CONNECTED_IN_A_ROW(CURRENT_HASH, CURRENT_HASH%RIGHT)
                CALL GENERATE_HASH(TYP1,ELEMENT_TYP1, TYP2,ELEMENT_TYP1, I+1, J, K, HASH_NUM, HASH_SIZE, CURRENT_HASH%RIGHT)
            END IF
        END IF
        

        IF (J-1<1) THEN
            CURRENT_HASH%BOTTOM => NULL()
        ELSE IF (.NOT. ASSOCIATED(CURRENT_HASH%BOTTOM)) THEN
            CALL FIND_HASH(I, J-1, K, CURRENT_HASH, NEW_HASH)
            IF (ASSOCIATED(NEW_HASH)) THEN
                CURRENT_HASH%BOTTOM => NEW_HASH
                NEW_HASH%TOP=>CURRENT_HASH
            ELSE
                CALL INITIALIZATION_HASH(TYP1,ELEMENT_TYP1,TYP2,ELEMENT_TYP1, I, J-1, K, HASH_SIZE, CURRENT_HASH%BOTTOM)
                CURRENT_HASH%BOTTOM%TOP=>CURRENT_HASH
                CALL CONNECTED_IN_A_ROW(CURRENT_HASH, CURRENT_HASH%BOTTOM)
                CALL GENERATE_HASH(TYP1, ELEMENT_TYP1, TYP2, ELEMENT_TYP1, I, J-1, K, HASH_NUM, HASH_SIZE, CURRENT_HASH%BOTTOM)
            END IF
        END IF
        
        IF (J+1>HASH_NUM) THEN
            CURRENT_HASH%TOP => NULL()
        ELSE IF (.NOT. ASSOCIATED(CURRENT_HASH%TOP)) THEN
            CALL FIND_HASH(I, J+1, K, CURRENT_HASH, NEW_HASH)
            IF (ASSOCIATED(NEW_HASH)) THEN
                CURRENT_HASH%TOP => NEW_HASH
                NEW_HASH%BOTTOM => CURRENT_HASH
            ELSE
                CALL INITIALIZATION_HASH(TYP1,ELEMENT_TYP1,TYP2,ELEMENT_TYP1, I, J+1, K, HASH_SIZE, CURRENT_HASH%TOP)
                CURRENT_HASH%TOP%BOTTOM => CURRENT_HASH
                CALL CONNECTED_IN_A_ROW(CURRENT_HASH, CURRENT_HASH%TOP)
                CALL GENERATE_HASH(TYP1,ELEMENT_TYP1, TYP2,ELEMENT_TYP1, I, J+1, K, HASH_NUM, HASH_SIZE, CURRENT_HASH%TOP)
            END IF
        END IF
        
        
        IF (K-1<1) THEN
            CURRENT_HASH%BACK => NULL()
        ELSE IF (.NOT. ASSOCIATED(CURRENT_HASH%BACK)) THEN
            CALL FIND_HASH(I, J, K-1, CURRENT_HASH, NEW_HASH)
            IF (ASSOCIATED(NEW_HASH)) THEN
                CURRENT_HASH%BACK => NEW_HASH
                NEW_HASH%FRONT=>CURRENT_HASH
            ELSE
                CALL INITIALIZATION_HASH(TYP1,ELEMENT_TYP1,TYP2,ELEMENT_TYP1, I, J, K-1, HASH_SIZE, CURRENT_HASH%BACK)
                CURRENT_HASH%BACK%FRONT=>CURRENT_HASH
                CALL CONNECTED_IN_A_ROW(CURRENT_HASH, CURRENT_HASH%BACK)
                CALL GENERATE_HASH(TYP1, ELEMENT_TYP1, TYP2, ELEMENT_TYP1, I, J, K-1, HASH_NUM, HASH_SIZE, CURRENT_HASH%BACK)
            END IF
        END IF
        
        IF (K+1>HASH_NUM) THEN
            CURRENT_HASH%FRONT => NULL()
        ELSE IF (.NOT. ASSOCIATED(CURRENT_HASH%FRONT)) THEN
            CALL FIND_HASH(I, J, K+1, CURRENT_HASH, NEW_HASH)
            IF (ASSOCIATED(NEW_HASH)) THEN
                CURRENT_HASH%FRONT => NEW_HASH
                NEW_HASH%BACK => CURRENT_HASH
            ELSE
                CALL INITIALIZATION_HASH(TYP1,ELEMENT_TYP1,TYP2,ELEMENT_TYP1, I, J, K+1, HASH_SIZE, CURRENT_HASH%FRONT)
                CURRENT_HASH%FRONT%BACK => CURRENT_HASH
                CALL CONNECTED_IN_A_ROW(CURRENT_HASH, CURRENT_HASH%FRONT)
                CALL GENERATE_HASH(TYP1,ELEMENT_TYP1, TYP2,ELEMENT_TYP1, I, J, K+1, HASH_NUM, HASH_SIZE, CURRENT_HASH%FRONT)
            END IF
        END IF
    END SUBROUTINE GENERATE_HASH
        
    
    
    
    SUBROUTINE FIND_HASH(I,J,K, CURRENT_HASH, TEMP_HASH)
        INTEGER :: I,J,K
        TYPE(HASH), POINTER :: CURRENT_HASH
        TYPE(HASH), POINTER :: TEMP_HASH
        
        ALLOCATE(TEMP_HASH)
        
        TEMP_HASH=>CURRENT_HASH%BEFORE
        
        DO WHILE (ASSOCIATED(TEMP_HASH))
            if (TEMP_HASH%X_INDEX == I .AND. TEMP_HASH%Y_INDEX == J .AND. TEMP_HASH%Z_INDEX == K) THEN
                RETURN
            END IF
            TEMP_HASH=>TEMP_HASH%BEFORE
        END DO
        
        TEMP_HASH=>CURRENT_HASH%NEXT
        DO WHILE (ASSOCIATED(TEMP_HASH))
            if (TEMP_HASH%X_INDEX == I .AND. TEMP_HASH%Y_INDEX == J .AND. TEMP_HASH%Z_INDEX == K) THEN
                RETURN
            END IF
            TEMP_HASH=>TEMP_HASH%NEXT
        END DO
        TEMP_HASH =>NULL()
        
    END SUBROUTINE
    
    
    
    SUBROUTINE CONNECTED_IN_A_ROW(CONNECTED_HASH, CURRENT_HASH)
        TYPE(HASH), POINTER :: CONNECTED_HASH
        TYPE(HASH), POINTER :: CURRENT_HASH
        TYPE(HASH), POINTER :: TEMP_HASH
        
        ALLOCATE(TEMP_HASH)
        
        TEMP_HASH=>CONNECTED_HASH
        
        DO WHILE (ASSOCIATED(TEMP_HASH%NEXT))
            TEMP_HASH=>TEMP_HASH%NEXT
        END DO
        CURRENT_HASH%BEFORE =>TEMP_HASH
        TEMP_HASH%NEXT => CURRENT_HASH
        
    END SUBROUTINE
END MODULE HASHING_3D

PROGRAM TEST
USE HASHING_3D
IMPLICIT NONE
INTEGER :: I, J, K, TYP1, ELEMENT_TYP1,  TYP2, ELEMENT_TYP2 , HASH_NUM
TYPE(HASH), POINTER :: NEW_HASH
REAL(8) :: SIZE
I = 1; J = 1; K = 1; TYP1 = 1; ELEMENT_TYP1=1;  TYP2=1; ELEMENT_TYP2=1
SIZE = 0.3
HASH_NUM = 10

NULLIFY(NEW_HASH)
CALL GENERATE_HASH(typ1,ELEMENT_TYP1, typ2,ELEMENT_TYP1, i, j, k, HASH_NUM, SIZE, NEW_HASH)

CALL DELETE_HASH(NEW_HASH)
WRITE(*,*) 'END'

END PROGRAM